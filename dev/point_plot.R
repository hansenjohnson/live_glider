# TESTING

# prep data ---------------------------------------------------------------

# read in data
D = readRDS('data-bond/detections.rds')
ctd = readRDS('data-bond/ctd.rds')

# date range
rng = range(ctd$date)

# date subset
DET = D[D$date >= rng[1] & D$date <= rng[2],]

# assign start and end times to each period
DET$time = as.POSIXct(DET$time)
DET$end = DET$time
DET$start = DET$end - 3600

# spp subsets

rw_p = DET[DET$right == 'present',]
rw_m = DET[DET$right == 'maybe',]
fw_p = DET[DET$fin == 'present',]
fw_m = DET[DET$fin == 'maybe',]
sw_p = DET[DET$sei == 'present',]
sw_m = DET[DET$sei == 'maybe',]
hw_p = DET[DET$humpback == 'present',]
hw_m = DET[DET$humpback == 'maybe',]

# point plot --------------------------------------------------------------

# layout
m = rbind(c(3,1,1,1,1,1,1,1,1,1,1,1,2),
          c(3,1,1,1,1,1,1,1,1,1,1,1,2))

layout(m)

# helper function
addPoints = function(wd, pos, pt_col, pt_pch = 21, cex = 2){
  
  x = wd$time
  y = rep(pos, length(x))
  
  points(x, y, pch = pt_pch, bg = pt_col, cex = cex)
}

spp = c('right', 'fin', 'sei', 'humpback')

rw_pos = 4.5
fw_pos = 4
sw_pos = 3.5
hw_pos = 3

# initiate blank plot
plot(DET$time, rep(length(spp), length(DET$time)), type = 'n', yaxt = 'n', ylab = '', xlab = '', ylim = c(2.5,5))
abline(h = c(rw_pos,fw_pos,sw_pos,hw_pos), col = 'grey')
addPoints(rw_m, rw_pos, 'yellow')
addPoints(rw_p, rw_pos, 'red')
addPoints(fw_m, fw_pos, 'yellow')
addPoints(fw_p, fw_pos, 'red')
addPoints(sw_m, sw_pos, 'yellow')
addPoints(sw_p, sw_pos, 'red')
addPoints(hw_m, hw_pos, 'yellow')
addPoints(hw_p, hw_pos, 'red')

axis(side = 2, at = c(rw_pos,fw_pos,sw_pos,hw_pos), tick = T, labels = c('right', 'fin', 'sei', 'humpback'), las = 2)

# rectangle plot ----------------------------------------------------------

spp = c('right', 'fin', 'sei', 'humpback')

plot(DET$time, rep(length(spp), length(DET$time)), type = 'n', yaxt='n', ylab = '', xlab = '')

xleft = rw_p$start
xright = rw_p$end
ytop = rep(5,length(xright))
ybottom = rep(4,length(xleft))

rect(xleft,ybottom,xright,ytop, col = 'yellow', border = 'yellow')

