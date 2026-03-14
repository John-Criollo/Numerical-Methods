import numpy as np
import matplotlib.pyplot as plt

def func(x,beta1 = 10.603060810094538, beta2 = 2.6314682225427188):
    return beta1/(x**5 * (np.exp(beta2/x)-1))

def main():
    input = np.loadtxt('12Class/sun_data.txt', dtype=float)
    # Generate t and compute function values
    x = np.linspace(0,7, 500)
    y = func(x)

    # Plot the analytic function
    plt.plot(x, y, label='Orginal function', ls='-',marker = '',color = 'b')
    # Plot the output data (use columns, not rows)
    plt.plot(input[:, 0], input[:, 1], label='Data 1', marker='.', ls='',color= 'k')
    #plt.plot(output1[:, 0], output1[:, 1], label='result2', marker='x', ls='',color = 'r')
    # Set axis limits and labels
    plt.xlim(0.1, 7)
    plt.ylim(0, 2)
    plt.xlabel('Lambda')
    plt.ylabel('B(lambda)')
    plt.legend()
    plt.title('Graph for the curve fitting')
    plt.show()

if __name__ == "__main__":
    main()

