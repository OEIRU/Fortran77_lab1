**Главная программа**
	program memory
	parameter(n=1000000)
	dimension A(n)
	common/razm/i,i1,i2
	call vvod(A(1),n)
	call multiplying(A(1),A(1+i),A(2*i),A(3*i-1),A(4*i-i1-1),
     *A(5*i-2*i1-1),A(6*i-2*i1-i2-1),A(7*i-2*i1-2*i2-1),
     *A(8*i-2*i1-2*i2-1),i,i1,i2)
	call output(A(8*i-2*i1-2*i2-1),i)
	stop
	end

**Ввод данных**
	subroutine vvod(A,n)
	common/razm/i,i1,i2
	dimension A(n)
	open(1,FILE='matrix.txt')	
*Ввод размерности матрицы и смещений диагоналей
	read(1,*)i,i1,i2
*Проверка матрицы на корректность
	if(i1.le.1.or.i2.le.i1.or.i.le.i1)then
		print*,'The matrix is given not correctly'
		stop
	endif
	open(2,FILE='vector.txt')	
*Ввод размерности вектора
	read(2,*)m
*Проверка вектора на коррекность
	if(i.ne.m)then
		print*,'The vector is given not correctly'
		stop
	endif
*Проверка на нехватку памяти 
	if(9*i-2*i1-2*i2-2>n)then 
		print*,'Shortage of memory'
		stop
	endif
	call readdata(A(1),A(1+i),A(2*i),A(3*i-1),A(4*i-i1-1),
     *A(5*i-2*i1-1),A(6*i-2*i1-i2-1),A(7*i-2*i1-2*i2-1),i,i1,i2)
	return
	end

**Ввод данных**
	subroutine readdata(A,B1,B2,C1,C2,D1,D2,E,k,l,m)	
	dimension A(k),B1(k-1),B2(k-1),C1(k-l),C2(k-l),D1(k-m),D2(k-m),E(k)	
*Ввод матрицы
	read(1,*)(A(i),i=1,k)
	read(1,*)(B1(i),i=1,k-1)
	read(1,*)(B2(i),i=1,k-1)
	read(1,*)(C1(i),i=1,k-l)
	read(1,*)(C2(i),i=1,k-l)
	read(1,*)(D1(i),i=1,k-m)
	read(1,*)(D2(i),i=1,k-m)
	close(1)
*Ввод вектора
	read(2,*)(E(i),i=1,k)	
	close(2)	
	return
	end
	
**Умножение матрицы на вектор**
	subroutine multiplying(A,B1,B2,C1,C2,D1,D2,E,F,k,l,m)
	dimension A(k),B1(k-1),B2(k-1),C1(k-l),C2(k-l)
	dimension D1(k-m),D2(k-m),E(k),F(k)
	F(1)=(A(1)+B1(1))*E(1)
	F(k)=(B2(k-1)+A(k))*E(k)	
	do i=2,k-1
		F(i)=(B2(i-1)+A(i)+B1(i))*E(i)
	end do
	do i=1,k-m
		F(i)=F(i)+(C1(i)+D1(i))*E(i)
	end do
	do i=i,k-l
		F(i)=F(i)+C1(i)*E(i)
	end do
	do i=l+1,m
		F(i)=F(i)+C2(i-l)*E(i)
	end do
	do i=i,k
		F(i)=F(i)+(D2(i-m)+C2(i-l))*E(i)
	end do
	return
	end

**Вывод результирующего вектора**
	subroutine output(A,k)
	dimension A(k)
	open(3,file='result.txt')
	write(3,*)(A(i),i=1,k)
	close(3)
	return
	end