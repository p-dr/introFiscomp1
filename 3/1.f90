program main

  implicit none
  real*8 x,h
  character c
  integer i
  x=1

  print*, exp(2*x)* (2*sin(x)+cos(x)), exp(2*x)*(3*sin(x)+4*cos(x))
  !========== 1 ===============================
  open(10, file="tab1.txt")
  write(10,*) "\begin{center}"//NEW_LINE(C)//"\begin{tabular}{|c|c | c| c|}"//NEW_LINE(C)//"\hline"
  write(10,*) "h & $f_f'(1)$ & $f_t'(1)$  & $f_{3s}'(1)$ \\"//NEW_LINE(C)//" \hline"
 
  do i = 1, 8
     h = 5.0d0 * 10.0d0**(-i)
     write(10,*) h, "&", dder(x,h),"&", eder(x,h),"&", der3(x,h),"\\ \hline"
     h = 10.0d0**(-i)
     write(10,*) h,"&", dder(x,h),"&", eder(x,h),"&", der3(x,h),"\\ \hline"
  end do

write(10,*) " \end{tabular}"//NEW_LINE(C)//"\end{center}"
close(10)

!============ 2 ===============================
open(10, file="tab2.txt")
  x=1
  write(10,*) "\begin{center}"//NEW_LINE(C)//"\begin{tabular}{|c|c | c| c|}"//NEW_LINE(C)//"\hline"
  write(10,*) "h & $f_{5s}'(1)$ & $f_{3s}''(1)$  & $f_{5s}''(1)$ \\"//NEW_LINE(C)//" \hline"
 
  do i = 1, 8
     h = 5.0d0 * 10.0d0**(-i)
     write(10,*) h, "&", der5(x,h),"&", der32(x, h),"&", der52(x, h),"\\ \hline"
     h = 10.0d0**(-i)
     write(10,*) h,"&", der5(x,h),"&", der32(x, h),"&", der52(x, h),"\\ \hline"
  end do

write(10,*) " \end{tabular}"//NEW_LINE(C)//"\end{center}"
close(10)

!======== 3 =================================
  open(10, file="tab3.txt")
  x=1
  write(10,*) "\begin{center}"//NEW_LINE(C)//"\begin{tabular}{|c|c | c| c|}"//NEW_LINE(C)//"\hline"
  write(10,*) "h & $f_f'(1)$ & $f_t'(1)$  & $f_{3s}'(1)$ \\"//NEW_LINE(C)//" \hline"
 
  do i = 1, 8
     h = 5.0d0 * 10.0d0**(-i)
     write(10,*) h, "&", e(x, dder(x,h)),"&", e(x, eder(x,h)),"&", e(x, der3(x,h)),"\\ \hline"
     h = 10.0d0**(-i)
     write(10,*) h,"&", e(x, dder(x,h)),"&", e(x, eder(x,h)),"&", e(x, der3(x,h)),"\\ \hline"
  end do

write(10,*) " \end{tabular}"//NEW_LINE(C)//"\end{center}"
close(10)

!====== 4 =======================================
open(10, file="tab4.txt")
  x=1
  write(10,*) "\begin{center}"//NEW_LINE(C)//"\begin{tabular}{|c|c | c| c|}"//NEW_LINE(C)//"\hline"
  write(10,*) "h & $f_{5s}'(1)$ & $f_{3s}''(1)$  & $f_{5s}''(1)$ \\"//NEW_LINE(C)//" \hline"
 
  do i = 1, 8
     h = 5.0d0 * 10.0d0**(-i)
     write(10,*) h, "&", e(x, der5(x,h)),"&", e2(x, der32(x, h)),"&", e2(x, der52(x, h)),"\\ \hline"
     h = 10.0d0**(-i)
     write(10,*) h,"&", e(x, der5(x,h)),"&", e2(x, der32(x, h)),"&", e2(x, der52(x, h)),"\\ \hline"
  end do

write(10,*) " \end{tabular}"//NEW_LINE(C)//"\end{center}"
close(10)


  contains

real*8 function f(x)
  real*8, intent(in) :: x
  f = dexp(2*x)*dsin(x)
  return
end function f

real*8 function dder(x, h)

  real*8, intent(in) :: x, h
  dder = (f(x+h)-f(x))/h
  return
end function dder

real*8 function eder(x, h)
  real*8, intent(in) :: x, h
  eder = (f(x)-f(x-h))/h
  return
end function eder

real*8 function der3(x,h)
  real*8, intent(in) :: x, h
  der3 = (f(x+h)-f(x-h))/(2*h)
  return
end function der3

real*8 function der5(x,h)
  real*8, intent(in) :: x, h
  der5 = (f(x-2*h)-8*f(x-h)+8*f(x+h)-f(x+2*h))/(12*h)
  return
end function der5

real*8 function der32(x,h)
  real*8, intent(in) :: x, h
  der32 = (f(x+h)-2*f(x)+f(x-h))/(h**2)
  return
end function der32

real*8 function der52(x,h)
  real*8, intent(in) :: x, h
  der52 = (-f(x-2*h)+16*f(x-h)-30*f(x)+16*f(x+h)-f(x+2*h))/(12*h**2)
  return
end function der52

real*8 function e(x, n)
  real*8, intent(in) :: x, n
  e = abs(exp(2*x)* (2*sin(x)+cos(x)) - n)
  return
end function e

real*8 function e2(x, n)
  real*8, intent(in) :: x, n
  e2 = abs(exp(2*x)*(3*sin(x)+4*cos(x)) - n)
  return
end function e2

end program main
