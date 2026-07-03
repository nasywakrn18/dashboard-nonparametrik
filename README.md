# dashboard-nonparametrik
Dashboard Uji Statistika Nonparametrik menggunakan R Shiny
# Dashboard Uji Statistika Non-Parametrik

## Deskripsi Aplikasi
Dashboard ini merupakan aplikasi interaktif berbasis R Shiny yang digunakan untuk membantu pengguna melakukan analisis statistika non-parametrik. Aplikasi ini memungkinkan pengguna mengunggah data dalam format CSV atau Excel, memilih jenis analisis, memilih metode uji, melihat hasil uji statistik, visualisasi data, serta kesimpulan otomatis.

## Tujuan Aplikasi
Tujuan dari aplikasi ini adalah mempermudah proses analisis data non-parametrik secara interaktif, terutama untuk data yang tidak memenuhi asumsi parametrik atau data yang berbentuk ordinal/ranking.

## Metode Statistika yang Digunakan
Metode uji yang tersedia dalam dashboard ini meliputi:

1. Wilcoxon One Sample Test
2. Mann-Whitney U Test
3. Wilcoxon Signed-Rank Test
4. Kruskal-Wallis Test
5. Friedman Test
6. Spearman Rank Correlation
7. Kendall's Tau Correlation
8. Runs Test / Wald-Wolfowitz

## Fitur Aplikasi
Beberapa fitur utama dalam aplikasi ini adalah:

- Upload data dalam format CSV atau Excel
- Preview data yang diunggah
- Statistik deskriptif variabel utama
- Informasi dataset
- Pemilihan tujuan analisis
- Pemilihan jumlah sampel atau variabel
- Pemilihan metode uji statistik
- Hasil uji non-parametrik
- Visualisasi data
- Kesimpulan otomatis berdasarkan p-value dan alpha

## Cara Menggunakan Aplikasi
1. Buka link aplikasi R Shiny.
2. Upload data dengan format CSV atau Excel.
3. Pilih tujuan analisis, yaitu Uji Perbedaan atau Uji Hubungan/Korelasi.
4. Pilih jumlah sampel atau variabel yang sesuai.
5. Pilih metode uji statistik.
6. Pilih variabel yang akan dianalisis.
7. Tentukan tingkat signifikansi atau alpha.
8. Klik tombol Eksekusi Analisis.
9. Lihat hasil uji, visualisasi, dan kesimpulan pada tab yang tersedia.

## Link Aplikasi R Shiny:
https://kompstat.shinyapps.io/dashboard-non-parametrik/

## Link Repository GitHub:
https://github.com/nasywakrn18/dashboard-nonparametrik

## Struktur Repository
Repository ini berisi source code aplikasi R Shiny dan file pendukung project.

```text
dashboard-nonparametrik/
├── app.R
├── README.md
└── .gitignore
