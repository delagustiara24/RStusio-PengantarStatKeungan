cicilan <- function(pilih, i, t, nilai, m = TRUE){
  j = i/m
  n = m*t
  v = (1+j)^-1
  #i: suku bunga
  #m: banyak bunga dibayar dalam 1 th
  #t: tahun
  #nilai: nilai akumulasi atau pv
  #pilih = 1, maka artinya cicilan unk nilai akumulasi akhir
  #pilih = 2, maka artinya cicilan unk nilai akumulasi awal
  #pilih = 3, maka artinya cicilan unk pv akhir
  #pilih = 4, maka artinya cicilan unk pv awal
  
  switch(pilih,
         satu = {
           bin = ((1+j)^n-1)/j
           cil = nilai/bin
           cat("cicilan unk nilai akumulasi akhir", cil)
         }, dua = {
           bin = ((1+j)^n-1)/(j*v)
           cil = nilai/bin
           cat("cicilan unk nilai akumulasi awal", cil)
         }, tiga = {
           bin = (1-v^n)/j
           cil = nilai/bin
           cat("cicilan unk pv akhir", cil)
         }, empat = {
           bin = (1-v^n)/(j*v)
           cil = nilai/bin
           cat("cicilan unk pv awal", cil)
         }
  )
}


