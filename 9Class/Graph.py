import numpy as np
import matplotlib.pyplot as plt

def func(x):
    return np.cos(x) + np.sin(x**2) - np.cosh(x**(-3))

def main():
    input = np.loadtxt('9Class/input_data.dat', dtype=float)
    input1 = np.loadtxt('9Class/input_data1.dat', dtype=float)
    output = np.loadtxt('9Class/output_data.dat', dtype=float)
    output1 = np.loadtxt('9Class/output_data1.dat', dtype=float)
    # Generate t and compute function values
    t = np.linspace(1, 100, 1000)
    y = func(t)

    # Plot the analytic function
    plt.plot(t, y, label='Orginal function', ls='-',marker = '.',color = 'y')
    # Plot the output data (use columns, not rows)
    plt.plot(input[:, 0], input[:, 1], label='Data 1', marker='.', ls='-',color= 'b')
    plt.plot(output[:, 0], output[:, 1], label='Result 1', marker='o', ls='',color= 'k')
    #plt.plot(output1[:, 0], output1[:, 1], label='result2', marker='x', ls='',color = 'r')
    # Set axis limits and labels
    plt.xlim(-10, 110)
    plt.ylim(-3.5, 2.5)
    plt.xlabel('t')
    plt.ylabel('F(t)')
    plt.legend()
    plt.title('Graph for the function and data file')
    plt.show()

if __name__ == "__main__":
    main()

