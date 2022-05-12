

mod_path = 'S:/main/data/qrf/gitrepo_data/output/modelFit/'

list.files(mod_path)


load(paste0(mod_path,list.files(mod_path)[11]))
chnk_rd = qrf_mod

load(paste0(mod_path,list.files(mod_path)[17]))
chnk_rd_cb = qrf_mod

mean(chnk_rd$mse)/mean(chnk_rd_cb$mse)
#combined has 4% higher MSE

load(paste0(mod_path,list.files(mod_path)[9]))
chnk_sum = qrf_mod

load(paste0(mod_path,list.files(mod_path)[13]))
chnk_sum_cb = qrf_mod

mean(chnk_sum$mse)/mean(chnk_sum_cb$mse)
#combined has 2.5% higher MSE

load(paste0(mod_path,list.files(mod_path)[12]))
stl_rd = qrf_mod
load(paste0(mod_path,list.files(mod_path)[18]))
stl_rd_cb = qrf_mod
mean(stl_rd$mse)/mean(stl_rd_cb$mse)
#combined has 1.8% higher MSE

load(paste0(mod_path,list.files(mod_path)[10]))
stl_sum = qrf_mod
load(paste0(mod_path,list.files(mod_path)[14]))
stl_sum_cb = qrf_mod
mean(stl_sum$mse)/mean(stl_sum_cb$mse)
#Same list of covs, so same MSE

#Check what dropping conductivity does to the winter models
load(paste0(mod_path,list.files(mod_path)[16]))
stl_win3 = qrf_mod
load(paste0(mod_path,list.files(mod_path)[22]))
stl_win4 = qrf_mod
mean(stl_win4$mse)/mean(stl_win3$mse)
#only 1% difference! 

load(paste0(mod_path,list.files(mod_path)[15]))
chk_win3 = qrf_mod
load(paste0(mod_path,list.files(mod_path)[21]))
chk_win4 = qrf_mod
mean(chk_win4$mse)/mean(chk_win3$mse)
#Only 0.6% difference



#"final" models vs first drafts
load(paste0(mod_path,list.files(mod_path)[3]))
chk_sum2 = qrf_mod

load(paste0(mod_path,list.files(mod_path)[31]))
chk_sum6 = qrf_mod
mean(chk_sum6$mse)/mean(chk_sum2$mse)
#only a 3% difference in MSE


load(paste0(mod_path,list.files(mod_path)[4]))
stl_sum2 = qrf_mod
load(paste0(mod_path,list.files(mod_path)[32]))
stl_sum6 = qrf_mod
mean(stl_sum6$mse)/mean(stl_sum2$mse)