Program basic_io
Implicit None
Real :: x,y
	Print*, 'Assesment N°1:'
	Print*, 'Computing value of z=sqrt(x^2 + y^2)'
	Print*, 'Enter x:'
	Read*, x
	Print*, 'Enter y:'
	Read*, y
	Print*, 'z is:', (x*x + y*y)**(0.5)
End Program basic_io

