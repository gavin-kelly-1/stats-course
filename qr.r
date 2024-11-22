library(qrcode)
code <- qr_code(commandArgs(TRUE)[1])
png(filename='r-images/quiz_qr.png', width=300, height=300)
plot(code)
dev.off()
