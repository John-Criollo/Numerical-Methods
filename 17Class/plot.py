import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("histo.txt")
plt.hist(data)
plt.show()
