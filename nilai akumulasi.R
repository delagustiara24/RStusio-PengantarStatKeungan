nilaiakumulasi <- function(pilih, k, t, im, m=TRUE){
  # satu : bunga tunggal
  # dua : bunga majemuk nominal
  # tiga : bunga majemuk kontinu
  # pilih: pilihan 1, 2, atau 3
  # k adalah uang pokok
  # t adalah tahun
  # im adalah tingkat suku bunga tunggal/ majemuk dalam desimal
  # m adalah banyak pembayaran bunga dalam 1 tahun
  j = im/m
  switch(pilih,
         satu = {
           ak = k*(1+j*t)
           cat("Nilai akumulasi bunga tunggal:", ak)
         },
         dua = {
           ak = k*(1+j)^(m*t)
           cat("Nilai akumulasi bunga majemuk nominal: ", ak)
         },
         tiga = {
           ak = k*exp(im*t)
           cat("Nilai akumulasi bunga kontinu: ", ak)
         }
  )
}

