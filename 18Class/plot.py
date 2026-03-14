import numpy as np
import matplotlib.pyplot as plt
import os
'''
data1 = np.loadtxt(os.path.expanduser("~/Documents/Workspace/18Class/histo1.txt"))
plt.title('1)Histogram of the means')
plt.xlabel('Means')
plt.hist(data1[:,0],20)
plt.show()

data2 = np.loadtxt(os.path.expanduser("~/Documents/Workspace/18Class/histo2.txt"))
plt.title('2)Trajectories ')
plt.xlabel('Number of steps')
plt.ylabel('Positions')
plt.plot(data2[:,0], data2[:,1])
plt.plot(data2[:,0], data2[:,2])
plt.plot(data2[:,0], data2[:,3])
plt.plot(data2[:,0], data2[:,4])
plt.plot(data2[:,0], data2[:,5])
plt.plot(data2[:,0], data2[:,6])
plt.plot(data2[:,0], data2[:,7])
plt.plot(data2[:,0], data2[:,8])
plt.plot(data2[:,0], data2[:,9])
plt.plot(data2[:,0], data2[:,10])
plt.show()
'''
data3 = np.loadtxt(os.path.expanduser("~/Documents/Workspace/18Class/histo3.txt"))
plt.title('3)Histogram of final positions')
plt.xlabel('Final positions')
plt.hist(data3,50)
plt.show()
