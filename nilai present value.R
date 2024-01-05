presentvalue <- function(pilih, A, t, im, m = TRUE){
  # satu : bunga tunggal
  # dua : bunga majemuk nominal
  # tiga : bunga majemuk kontinu
  # pilih: pilihan 1, 2, atau 3
  # A adalah nilai akumulasi
  # t adalah tahun
  # m adalah banyak pembayaran bunga dalam 1 tahun
  switch(pilih,
         satu = {
           pv = A/(1+im*t)
           cat("Nilai pv bunga tunggal: ", pv)
         },
         dua = {
           pv = A*(1+im/m)^(-m*t)
           cat("Nilai pv bunga majemuk nominal: ",
                pv)
         },
         tiga = {
           pv = A*exp(-im*t)
           cat("Nilai pv bunga kontinu: ", pv)
         }
  )
}


