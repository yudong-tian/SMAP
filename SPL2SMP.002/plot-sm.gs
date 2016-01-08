'open soil_moisture.ctl' 
'set gxout shaded' 
'set grads off'

it=1
while (it <= 13) 
  'set t 'it 
  'set clevs 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65' 
  'd sm' 
  it=it+1
endwhile 
'cbarn' 
'draw title SPL2SMP.002 for 12.31.2015' 
'gxyat -x 1000 -y 700 sm.png' 
'quit'



