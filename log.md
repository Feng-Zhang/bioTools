本文档用来记录更新及待做的事。

- [x] fix_20211206_survivalstatus : clean_phe中vival_status被编码成1和2，应该编码成1和0. 1代表死亡，2代表生存。 tumor_expr的id只有12位，与all_expr不一致。因此添加了两个新数据框，存放btcga_i, bar_patient_barcode, type信息，并去重复。
- [ ] 
