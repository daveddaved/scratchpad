from random import shuffle


def bubble_sort(input_array):
    passes = -1
    size = len(input_array) - 1
    while not passes == 0:
        passes = 0
        start = 0
        while start < size:
            if input_array[start] > input_array[start + 1]:
                swap(start, start + 1, input_array)
                passes += 1

            start += 1
    return input_array


def swap(x, y, arr):
    temp = arr[y]
    arr[y] = arr[x]
    arr[x] = temp

def shift(x, y, arr):
    swap(x, y - 1, arr)
    swap(y - 1, y, arr)


def merge(xs, ys, el):
    xs.extend(el)
    return xs + ys


def quick_sort(array):
    _quicksort(array, 0, len(array) - 1)
    return array


def _quicksort(array, start, stop):
    if stop - start > 0:
        pivot, left, right = array[start], start, stop
        while left <= right:
            while array[left] < pivot:
                left += 1
            while array[right] > pivot:
                right -= 1
            if left <= right:
                array[left], array[right] = array[right], array[left]
                left += 1
                right -= 1
        _quicksort(array, start, right)
        _quicksort(array, left, stop)


test_list2 = [1, 2, 3]
test_list22 = [2, 1]
test_list3 = [1, 3, 2]
test_list1 = [1]
test_list0 = []
test_list4 = [[i] for i in range(10)]
shuffle(test_list4)

print test_list0
print quick_sort(test_list0)
# # print bubble_sort(test_list0)
print test_list1
print quick_sort(test_list1)
print test_list22
print quick_sort(test_list22)
print quick_sort([9, 3, 1, 4])
#
#print bubble_sort(test_list1)
print test_list2
print quick_sort(test_list2)
print test_list3
print quick_sort(test_list3)
print test_list4
print quick_sort(test_list4)
# print bubble_sort(test_list4)
# print quicksort(test_list0)
# print quicksort(test_list1)
# print quicksort(test_list4)
# test_list5 = [1,0, 3, 2]
# print sort(0, len(test_list5) - 1, test_list5)
# print test_list5


