'open tbs.ctl'
'set gxout shaded' 
'set grads off'

it=1
while (it <= 25) 
  'set t 'it 
  'set clevs 80 100 120 140 160 180 200 220 240 260' 
  'd tbh' 
  it=it+1
endwhile 
'cbarn' 
'draw title SPL1CTB.002, Tb_H for 12.31.2015' 
'gxyat -x 1000 -y 700 tbh.png' 
'quit'



