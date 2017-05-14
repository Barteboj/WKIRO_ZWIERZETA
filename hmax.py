# -*- coding: utf-8 -*-
"""
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

"""
import cv2
import numpy as np
#from sklearn.externals.joblib import delayed, Parallel
from numpy.lib.stride_tricks import as_strided

"""
Lista zmian w stosunku do pracy dyplomowej:
hmax_array_s2_c2 zastąpiło hmax_parallel_s2_c2 [wersja równoległa powodowała problemy na Windowsie]
deskew_rotation - zawsze wykonuje jeden obrót
"""

def hmax_create_filters(filter_sizes, filter_angles):
    """
    Generuje filtry używane przez warstwę s1
    filter size - jednowymiarowa tablica/krotka rozmiarów filtrów
    filter_angles - jednowymiarowa tablica/krotka rozptrywanych kątów prze każdy z rozmiarów
    """
    filters = []
    for i in filter_sizes:
        actual = []
        lambd=i*2/(4-((i-7)/2*0.05))
        indexMax = float(i/2)
        xx, yy = np.mgrid[-indexMax:indexMax+1, -indexMax:indexMax+1]
        xx=cv2.multiply(xx,xx)
        yy=cv2.multiply(yy,yy)
        toZero = cv2.add(xx,yy)
        toZero = toZero>(i*0.5)**2
        for j in filter_angles:
            newFilter=cv2.getGaborKernel((i,i),0.8*lambd,j, lambd, 0.3, 0.)
            newFilter[toZero] = 0
            newFilter=cv2.subtract(newFilter, cv2.mean(newFilter))
            newFilter=cv2.divide(newFilter, 
                                 np.sqrt(
                                 cv2.sumElems(
                                 cv2.multiply(newFilter,newFilter))[0]))
            actual.append(newFilter)
        filters.append(actual)
    return filters


def hmax_s1(image, filters, with_edge=False):
    """
    Generuje odpowiedź warstwy s1 - pierwszy wymiar to rozmiar, drugi to kąt, 3 i 4 współrzędne
    image - obraz wejściowy, powinien być typu numpy.array [2 wymiary] [zakres wartości 0.0-1.0]
    filters - firtry wygenerowane przez hmax_create_filters
    with_edge - czy obszary znajdujące się zbyt blisko krawędzi by filtr zmieścił się w obrazie mają być wyliczane
    """
    imageSqr = cv2.multiply(image, image)
    resultShape = (len(filters), len(filters[0])) + image.shape
    result = np.empty(resultShape, image.dtype)
    tmp = np.empty_like(image)
    toDivide=np.empty_like(image)
    
    for i in xrange(len(filters)):
        filterShape = filters[i][0].shape

        edge = filterShape[0]/2

        cv2.boxFilter(imageSqr, 
                      -1, 
                      filterShape,
                      toDivide,
                      normalize=False, 
                      borderType=cv2.BORDER_CONSTANT)
        toDivideMask = toDivide <= 1e-14
        toDivide[toDivideMask] = 1.0
        cv2.sqrt(toDivide, toDivide)
        for j in xrange(result.shape[1]):
            cv2.filter2D(image,
                         -1, 
                         filters[i][j],
                         tmp, 
                         borderType=cv2.BORDER_CONSTANT)
            tmp[toDivideMask]=0.0
            cv2.divide(tmp,           
                       toDivide, 
                       result[i][j]) 
        if not with_edge:
            result[i, :, 0:edge, :] = 0.0
            result[i, :, -edge:, :] = 0.0
            result[i, :, :, 0:edge] = 0.0
            result[i, :, :, -edge:] = 0.0
    result = np.abs(result)
    return result
    

def hmax_c1(s1, to_merge, overlap_factor = 2):
    """
    Generuje odpowiedź warstwy c1 [4 wymiary, gdzie pierwszy jest przyporządkowany jednemu elementowi z to_merge]
    to_merge - struktura typu [[[0,1],8], [[2,3],10], [[4,5],12], [[6,7],14], [[8,9],16], [[10,11],18]]
    gdzie: [[0,1],8] należy czytać jako - dla pierwszej warstwy s1 i kolejnej przyjmij okno o rozmiarze 8
    """
    result = np.empty(len(to_merge), np.object)
    
    #print result.shape

    for i in xrange(len(to_merge)):
        indexes  = to_merge[i][0]
        size = to_merge[i][1]
        band = s1[indexes[0]]
        for j in xrange(indexes[0]+1, indexes[1]+1):
            band = cv2.max(band,s1[j])

        structuringElement1 = np.ones((size, 1), np.uint8)
        structuringElement2 = np.ones((1, size), np.uint8)        
        step = int(np.ceil(size/overlap_factor))

        c1Response = np.empty_like(band[::, ::step, ::step])
        
        for j in xrange(np.size(band,0)):

            tmpMax = cv2.dilate(  band[j],
                                structuringElement1,
                                anchor=(0,0),
                                borderType=cv2.BORDER_CONSTANT,
                                borderValue=0.0)
            tmpMax = cv2.dilate( tmpMax[::step],
                                 structuringElement2,
                                 anchor=(0,0),
                                 borderType=cv2.BORDER_CONSTANT,
                                 borderValue=0.0)
            c1Response[j] = tmpMax[:,::step]
        result[i] = c1Response
    return result


def hmax_c1_to_vector(c1):
    """ Przekształca c1 w wektor wartości"""
    result = c1[0].flatten()
    for i in range(1, len(c1)):
        result = np.append(result, c1[i].flatten(),0)
    return result


def hmax_s2_c2(c1, dictionary, dictionary_constants):
    """
    wylicza wartości warstwy c2 [wektor]
    NIE UŻYWAĆ TEJ FUNKCJI SAMODZIELNIE
    """
    bandsLength = len(c1)    
    counter=0    
    for i in dictionary:
        counter+=i.shape[0]
    result = np.empty((bandsLength, counter))

    counter=0
    
    tmpBands = np.empty(bandsLength, np.object)      
    for i in xrange(bandsLength):
        tmp = cv2.multiply(c1[i], c1[i])
        for j in xrange(1, tmp.shape[0]):
            cv2.add(tmp[0], tmp[j], tmp[0])
        tmpBands[i] = tmp[0]
    for patches in dictionary:   
        a = np.empty(bandsLength, np.object)
        kernelShape = patches.shape[2:]
        for i in xrange(bandsLength):
            a[i] = cv2.boxFilter(tmpBands[i], -1, kernelShape,
                                 normalize=False, 
                                 borderType=cv2.BORDER_CONSTANT)
        patchShape = patches[0].shape[0]
        for patch in patches:
            
            for i in xrange(bandsLength):
                
                band = c1[i]
                
                bandResult = cv2.filter2D(band[0], -1, patch[0],
                                         borderType=cv2.BORDER_CONSTANT)
                                         
                for layer in xrange(1, patchShape):
                    cv2.add(bandResult,
                            cv2.filter2D(band[layer], -1, patch[layer],
                                         borderType=cv2.BORDER_CONSTANT),
                                         bandResult)
                
                cv2.multiply(bandResult, -2.0, bandResult)
                
                cv2.add(bandResult, a[i], bandResult)

                result[i, counter]=cv2.minMaxLoc(bandResult)[0]
                
            counter+=1
    for i in xrange(1, result.shape[0]):
        cv2.min(result[0], result[i], result[0])
    return np.ravel(cv2.add(result[0],dictionary_constants))


def hmax_array_s2_c2(c1, dictionary):
    """
    wylicza wartości warstwy c2
    c1 - lista wyników z warstwy c1
    dictionary - słownik zgodny z tworzonym przez hmax_select_random_patches
    """
    b = np.array([(i*i).sum((-3,-2,-1)) for i in dictionary]).flatten()    
    a = [hmax_s2_c2(i, dictionary, b) for i in c1]
    return np.array(a)

#powoduje porblemy na windowsach
#def hmax_parallel_s2_c2(c1, dictionary):
#    b = np.array([(i*i).sum((-3,-2,-1)) for i in dictionary]).flatten()    
#    a = Parallel(-1, verbose=0)(delayed(hmax_s2_c2)(i, dictionary, b) for i in c1)
#    return np.array(a)


def hmax_get_all_patches(c1, patch_size):
    """
    Zwraca wszystkie fragmenty warstwy c1 o rozmiarze patch_size
    """
    result = np.empty((0,c1[0].shape[0],patch_size[0],patch_size[1]))    
    for band in c1:
        if band.shape[1]>=patch_size[0] and band.shape[2]>=patch_size[1]:
            z = band.shape[0]
            y = band.shape[1]
            x = band.shape[2]
            part = np.lib.stride_tricks.as_strided(band,
                                                      (y-patch_size[0]+1,
                                                       x-patch_size[1]+1,
                                                        z,
                                                     patch_size[0],
                                                     patch_size[1]),
                                                     (band.strides[1],
                                                      band.strides[2],
                                                      band.strides[0],
                                                      band.strides[1],
                                                      band.strides[2]))
            part = part.reshape((part.shape[0]*part.shape[1], 
                                 part.shape[2], 
                                 part.shape[3], 
                                 part.shape[4]))
            result=np.append(result, part, 0)
    return result


def hmax_select_random_patches(c1_list, patch_size, to_select=1):
    """
    wybiera z każdego elementu c1_list dokładnie to_select elementów o romiarze patch_size
    c1_list - lista odpowiedzi z warstwy c1 dla różnych obrazów
    patch_size - dwuwymiarowa lista/krotka zawierająca rozmiar
    to_select - ile wybrać dla danego elementu c1_list
    """
    result=[]
    for i in c1_list:
        patches = hmax_get_all_patches(i, patch_size)
        for i in range(to_select):
            index = np.random.randint(0, patches.shape[0])
            result.append(patches[index])
    return np.array(result)    


def deslain(image):
    """
    jedna z metod "prostowania" znaków
    """
    yy, xx = np.mgrid[1:image.shape[0]+1, 1:image.shape[1]+1]
    x_sr = (xx*image).sum()/image.sum()
    y_sr = (yy*image).sum()/image.sum()
    m = ((xx*yy*image).sum()-x_sr*y_sr*(image.sum()))/(
        (yy*yy*image).sum() - y_sr*y_sr*(image.sum()))
    x_prim = xx+m*(yy-y_sr)
    x_prim[x_prim<1]=1
    x_prim[x_prim>image.shape[1]]=image.shape[1]

    wyn = (np.ceil(x_prim)-x_prim)*image[
    yy-1,np.floor(x_prim).astype(np.int)-1]+(x_prim - np.floor(x_prim))*image[
    yy-1, np.ceil(x_prim).astype(np.int)-1]
    wyn+= (np.ceil(x_prim)==np.floor(x_prim))*image[yy-1, x_prim.astype(np.int)-1]
    return wyn
  
  
def rotate_image(image, angle):
    """obraca obraz o zadany kąt"""
    x,y = image.shape
    mat = cv2.getRotationMatrix2D(((x-1)/2.0, (y-1)/2.0), angle, 1)
    return cv2.warpAffine(image, mat, (x,y))


def skew_angle(image):
    """zwraca nachylenie obrazu"""
    yy, xx = np.mgrid[1:image.shape[0]+1, 1:image.shape[1]+1]
    x_sr = (xx*image).sum()/image.sum()
    y_sr = (yy*image).sum()/image.sum()
    m = ((xx*yy*image).sum()-x_sr*y_sr*(image.sum()))/(
        (yy*yy*image).sum() - y_sr*y_sr*(image.sum()))
    return np.degrees(np.arctan(m))
    
def deskew_rotation(image):
    """obraca obraz tak by zminimalizować jego nachylenie"""
    tmp = image
    angle = skew_angle(tmp)
    tmp = rotate_image(tmp, -angle)
    return tmp


def hmax_merge_orientations(s1, with_rotation = True):
    """metoda łączenia orientacji - patrz praca dyplomowa"""
    result = np.empty_like(s1)
    angle = 180.0/result.shape[1]

    for i in range(result.shape[0]):
        for j in range(result.shape[1]-1):
            tmp1 = result[i,j-1]
            tmp2 = result[i,j+1]
            if with_rotation:
                tmp1 = rotate_image(tmp1, angle)
                tmp2 = rotate_image(tmp2, -angle)
            result[i,j] = np.maximum(tmp1, s1[i][j])
            result[i,j] = np.maximum(tmp2, result[i,j])
        tmp1 = s1[i,-2]
        tmp2 = s1[i,0]
        if with_rotation:
            tmp1 = rotate_image(tmp1, angle)
            tmp2 = rotate_image(tmp2, -angle)
        result[i,-1] = np.maximum(tmp1, s1[i,-1])
        result[i,-1] = np.maximum(tmp2, result[i,-1])
    return result

#funckje dla metody Teowa i Loe'go
def teow_create_filters():
    main_filters = []
    basic_filter = np.array(   [[-1, 0, 1, 0, 0],
                    [-1, 0, 1, 0, 0],
                    [-1, 0, 1, 0, 0],
                    [-1, 0, 1, 0, 0],
                    [-1, 0, 1, 0, 0]])
    main_filters.append(basic_filter)
    basic_filter = np.array(   [[0, 0, -2, 0, -1],
                    [0, -1, 0, 1, 0],
                    [-2, 0, 1, 0, 0],
                    [0, 1, 1, 0, 0],
                    [1, 0, 0, 0, 0]])
    main_filters.append(basic_filter)
    basic_filter = np.array(   [[0, -8, -8, -8, 0],
                    [-1, 0, 2, 0, -1],
                    [-1, 0, 2, 0, -1],
                    [-1, 0, 2, 0, -1],
                    [-1, 0, 2, 0, -1]])
    main_filters.append(basic_filter)
    basic_filter = np.array(   [[0, 0, -1, -8, -8],
                    [0, -1, 0, 2, -8],
                    [-2, 0, 2, 0, -1],
                    [0, 2, 0, -1, 0],
                    [2, 0, -2, 0, 0]])
    main_filters.append(basic_filter)
    result = []
    for i in main_filters:
        result.append(i)
        for j in range(1,4):
            result.append(np.rot90(i, j))
    return result   


def teow_s(image, filters, with_negative = True):
    result = []
    for i in filters:
        g = cv2.filter2D(image,
                         -1, 
                         i)
        g[g<0.0]=0
        q=image*g
        result.append(q[i.shape[0]/2:-i.shape[0]/2+1, 
                        i.shape[1]/2:-i.shape[0]/2+1])
    if with_negative:
        result.extend(teow_s(1.0-image, filters, False))
    return result
    
def teow_c(s, step=3, window_size=8, fi=1):
    toReturn=[]    
    for image in s:
        if image.shape[0]>window_size and image.shape[1]>window_size:
            x, y = image.shape
            x1 = (x-window_size)/step+1
            y1 = (y-window_size)/step+1
            new_q = as_strided(image, [x1, y1, window_size, window_size], 
                               [image.strides[0]*step, 
                                image.strides[1]*step, 
                                image.strides[0], 
                                image.strides[1]])
            
            q_power = np.abs(new_q)**fi
            sum_q_power = q_power.sum((-1,-2))
            sum_q_power[sum_q_power<=0]=1.0
            result = (q_power*new_q).sum((-1,-2))/sum_q_power
            toReturn.append(result)
    return np.array(toReturn).flatten()