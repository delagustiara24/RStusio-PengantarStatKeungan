obligasi <- function(f, r, i, t, m){
  # Fungsi untuk menentukan harga obligasi
  # f: face value, r: kupon (dalam desimal), i: yield
  # t: umur obligasi (tahun), m: banyak pemberian kupon dalam 1 tahun
  
  n = t*m
  rbin = r/m
  ibin = i/m
  v = (1+ibin)^-1
  an = (1-v^n)/ibin
  
  P = f*rbin*an + f*v^n
  cat("Harga obligasi: ", P)
  
}
