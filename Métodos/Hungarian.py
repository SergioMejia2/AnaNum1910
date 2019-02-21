import numpy as np
from scipy.optimize import linear_sum_assignment
matrix = np.array([[4,1,3],[2,0,5],[3,2,2]])

row_id, col_id = linear_sum_assignment(matrix)

print(row_id, col_id )

cost = matrix[row_id, col_id].sum()

print(cost)
