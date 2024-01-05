annuitas <- function (pilih, k, t, i, m = TRUE)
{
  
  # pilih 1, 2
  # 1: annuitas akhir
  # 2: annuitas awal
  # k adalah uang pokok
  # t adalah tahun
  # i adalah besaran bunga (dalam desimal)
  # m adalah banyak pembayaran bunga dalam 1 tahun
  n = m*t
  j = i/m
  v = (1+j)^-1
  switch(pilih,
         satu = {
           an = k*(1-v^n)/j
           sn = k*((1+j)^n-1)/j
           cat("pv akhir: ", an,"\n", "sn akhir: ",
               sn)
         }, dua = {
           an = k*(1-v^n)/(j*v)
           sn = k*((1+j)^n-1)/(j*v)
           cat("pv awal: ", an,"\n", "sn awal: ",
               sn)
         })
  
}
12