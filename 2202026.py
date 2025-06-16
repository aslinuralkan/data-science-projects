import numpy as np

arr = np.loadtxt('quiz1.txt')

print(arr.shape)
print(arr.dtype)

reshaped_arr = arr.reshape(2, 10)
print(reshaped_arr)

arr[arr > 10] = -1
print(arr)

second_row = arr[1, :]
last_column = arr[:, -1]
top_left_2x2 = arr[:2, :2]

print(second_row)
print(last_column)
print(top_left_2x2)

mean_value = np.mean(arr)
std_dev = np.std(arr)
max_value = np.max(arr)
min_value = np.min(arr)

print(mean_value)
print(std_dev)
print(max_value)
print(min_value)