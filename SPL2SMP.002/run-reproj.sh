
# reproject 1-day's worth of data

i=0 
for ifile in `ls 2015.12.31/*.h5`; do 
  hr=`printf "%02d" $i`
  ./reproj $ifile sm$hr.1gd4r
  let i=i+1
  #echo $hr
done 
  

