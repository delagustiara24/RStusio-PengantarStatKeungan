amortisasi <- function(an, i, t, m = TRUE, k = TRUE){
  n = m*t
  j = i/m
  v = (1+j)^-1
  
  if (is.null(an)){
    an = k*(1-v^n)/j
  }
  
  k = round(an*j/(1-v^n), digits = 3)
  p = n+1
  A = array (NA, p)
  B = array (NA, p)
  C = array (NA, p)
  D = array (NA, p)
  D[1] = an
  
  for (i in 2:p){
    A[i] = k
    B[i]= round(j*D[i-1], digits=3 )
    C[i] = round(A[i]-B[i], digits=3)
    D[i] = round(D[i-1]-C[i], digits=3)
  }
  jadwal = data.frame(A, B, C, D)
  return(jadwal)
}


aa = amortisasi(100, 0.06, 20, 12)
bb = amortisasi(NULL, 0.12, 5, 4, 8)
aa
bb

amortisasi(100, 0.06, 20, m = 12, k = TRUE)
sum()

hitung = amortisasi(100, 0.06, 20, m = 12)
hitung
bunga_total = sum(hitung$B, na.rm = TRUE)
bunga_total
plot(hitung$B)
plot(hitung$C)
hitung = amortisasi(NULL, 0.12, 5, m = 4, k =8)
hitung
