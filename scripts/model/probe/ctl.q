\c 60 200
S: first .z.x
t: raze {[id] p:get hsym `$S,"/pr",(string id),"/probe.bin"; b:p 5; b:b where 4000<b[;0];
  c: `float$b[;10]; enlist `id`thresh`scope`mag`stratOn`align`nuse!(id;avg `float$b[;1];avg `float$b[;2];avg `float$b[;3];avg 0<b[;7];avg c where not null c;avg `float$b[;11])} each 1+til 16;
show t
