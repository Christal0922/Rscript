#建立向量,可为字符型，数值型或逻辑型#
a <- c(1,2,3,4)
b <- c("one","two")

#建立矩阵#
mymatrix <- matrix(vector,nrow=number_of_rows,ncol=number_of_columns,
                   byrow=logical_value,dimnames=list(char_vector_rownames,char_vector_colnames)) #默认按列排列#
cells <- c(1,26,24,68)
rnames <- c("R1","R2")
cnames <- c("c1","c2")
mymatrix <- matrix(cells,nrow=2,ncol=2,byrow=TRUE,dimnames=list(rnames,cnames))

#建立空白矩阵#
x <- rep(0,15)
dim(x) <- c(3,5)
x

x <- matrix(0,3,5)

#建立数组#
myarray <- array(vector,dimensions, dimnames)
dim1 <- c("A1","A2")
dim2 <- c("B1","B2","B3")
dim3 <- c("C1","C2","C3","C4")
Z <- array(1:24,c(2,3,4),dimnames=list(dim1,dim2,dim3))
#dimensions为数值型向量，给出了各个维度下标的最大值，例如dim1最大只能容纳2个值，dim2为3个值，dim3为4个值#

#建立数据框#
mydata <- data.frame(col1,col2,col3)

#attach(),将数据框添加到R的搜索路径中，detach（）将数据框从搜索路径中移除#
summarY(mtcars$mpg)
plot(mtcars$mpg,mtcars$disp)
#也可用attach(),detach()#
attach(mtcars)
summary(mpg)
plot(mpg,disp)
detach(mtcars)
#也可用with#
with(mtcars,{
  print(summary(mpg))
  plot(mpg,disp)
})

#因子#
#变量可归结为名义型、有序型或连续型变量，类别（名义型）和有序类别（有序型）在R中称为因子#
#函数factor()以一个整数向量的形式存储类别值，整数的取值范围是[1...k]#
status <- c("Poor","improved","Excellent","Poor")
status <- factor(status,order=TRUE,levels=c("Poor","Improved","Excellent"))
sex <- factor(sex,levels=c(1,2),labels=c("Male","Female"))
#函数str(object)提供R中某个对象（如数据框）的信息#

#列表#
mylist <- list(object1,object2)

