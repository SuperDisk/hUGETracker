procedure add16(Var first:Word;second:Word);
begin
	qtemp.Q:=longint(first)+longint(second);
	first:=qtemp.w1;
	af.l:=(af.l and $C4) OR (qtemp.b2 and $38);
	if qtemp.b3=0 then af.l:=af.l and 254 (* reset Cf *) else
        af.l:=af.l or 1;(* set Cf *)
end;

procedure adc16(first,second:Word);
begin
	qtemp.Q:=longint(first)+longint(second)+(af.l and 1);
	af.l:=(qtemp.b2 and $B8);
	if qtemp.b3=0 then af.l:=af.l and 254 (* reset Cf *) else af.l:=af.l or 1;(* set Cf *)
	if qtemp.w1>0 then af.l:=af.l and 191 (* reset Zf *) else af.l:=af.l or 64;(* set Zf *)
	if ((first XOR qtemp.w1) SHR 15>0) and
           ((first XOR second) SHR 15=0) then af.l:=af.l or 4 (* set Pf *) else
            af.l:=af.l and 251;(* reset Pf *)
	hl.W:=qtemp.w1;
end;

procedure sbc16(first,second:Word);
begin
	qtemp.Q:=longint(first)-longint(second)-(af.l and 1);
	af.l:=(qtemp.b2 and $B8) OR 2;
	if qtemp.b4=0 then af.l:=af.l and 254 (* reset Cf *) else af.l:=af.l or 1;(* set Cf *)
	if qtemp.w1>0 then af.l:=af.l and 191 (* reset Zf *) else af.l:=af.l or 64;(* set Zf *)
	if ((first XOR qtemp.w1) SHR 15>0) and
           ((second XOR qtemp.w1) SHR 15=0) then af.l:=af.l or 4 (* set Pf *) else
         af.l:=af.l and 251 ;(* reset Pf *)
	hl.W:=qtemp.w1;
end;

procedure add8(Lo,Pls:Byte);
begin
	ptemp.W:=Lo+Pls;
	if ptemp.h=0 then
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l and 254; (* reset Cf *)
	end
	else
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l or 1;(* set Cf *)
	end;
        ptemp.l:=ptemp.l and 253;(* reset Nf *)
	if ptemp.h>0 then
         ptemp.l:=ptemp.l and 191 (* reset Zf *)
        else
         ptemp.l:=ptemp.l or 64;(* set Zf *)
	if ((Lo XOR Pls XOR ptemp.h) and 16>0) then
        ptemp.l:=ptemp.l or 16 (* set Hf *)
        else
        ptemp.l:=ptemp.l and 239;(* reset Hf *)
	if ((Lo XOR ptemp.h) SHR 7>0) and  ((Lo XOR Pls) SHR 7=0) then
        ptemp.l:=ptemp.l or 4 (* set Pf *)
        else
        ptemp.l:=ptemp.l and 251;(* reset Pf *)
	af.W:=ptemp.W;
end;

procedure sub8(Lo,Mns:Byte);
begin
	ptemp.W:=Lo-Mns;
	if ptemp.h=0 then
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l and 254; (* reset Cf *)
	end
	else
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l or 1;(* set Cf *)
	end;
        ptemp.l:=ptemp.l or 2; (* set Nf *)
	if ptemp.h>0 then
         ptemp.l:=ptemp.l and 191 (* reset Zf *)
        else
         ptemp.l:=ptemp.l or 64;(* set Zf *)
	if (Lo XOR Mns XOR ptemp.h) and 16>0 then
        ptemp.l:=ptemp.l or 16 (* set Hf *)
        else
        ptemp.l:=ptemp.l and 239;(* reset Hf *)
	if ((Lo XOR ptemp.h) SHR 7>0) and ((Mns XOR ptemp.h) SHR 7=0)
        then ptemp.l:=ptemp.l or 4 (* set Pf *)
        else ptemp.l:=ptemp.l and 251;(* reset Pf *)
	af.W:=ptemp.W;
end;

procedure adc8(Lo,Pls:Byte);
begin
	ptemp.W:=Lo+Pls+(af.l and 1);
	if ptemp.h=0 then
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l and 254; (* reset Cf *)
	end
	else
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l or 1;(* set Cf *)
	end;
        ptemp.l:=ptemp.l and 253;(* reset Nf *)
	if ptemp.h>0 then
        ptemp.l:=ptemp.l and 191(* reset Zf *)
        else
        ptemp.l:=ptemp.l or 64;(* set Zf *)
	if (Lo XOR Pls XOR ptemp.h) and 16>0 then
        ptemp.l:=ptemp.l or 16 (* set Hf *)
        else
        ptemp.l:=ptemp.l and 239;(* reset Hf *)
	if ((Lo XOR ptemp.h) SHR 7>0) and ((Lo XOR Pls) SHR 7=0) then
         ptemp.l:=ptemp.l or 4 (* set Pf *)
        else ptemp.l:=ptemp.l and 251;(* reset Pf *)
	af.W:=ptemp.W;
end;

procedure sbc8(Lo,Mns:Byte);
begin
	ptemp.W:=Lo-Mns-(af.l and 1);
	if ptemp.h=0 then
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l and 254; (* reset Cf *)
	end
	else
	begin
		ptemp.h:=ptemp.l;
                ptemp.l:=ptemp.l or 1;(* set Cf *)
	end;
        ptemp.l:=ptemp.l or 2; (* set Nf *)
	if ptemp.h>0 then
        ptemp.l:=ptemp.l and 191 (* reset Zf *)
        else
        ptemp.l:=ptemp.l or 64;(* set Zf *)
	if (Lo XOR Mns XOR ptemp.h) and 16>0 then
        ptemp.l:=ptemp.l or 16 (* set Hf *)
        else
        ptemp.l:=ptemp.l and 239;(* reset Hf *)
	if ((Lo XOR ptemp.h) SHR 7>0)  and ((Mns XOR ptemp.h) SHR 7=0)
        then
         ptemp.l:=ptemp.l or 4 (* set Pf *)
        else ptemp.l:=ptemp.l and 251;(* reset Pf *)
	af.W:=ptemp.W;
end;

procedure cp_sub8(Lo,Mns:Byte);
begin
	ptemp.W:=Lo-Mns;
	af.l:=(Mns and 40) OR (ptemp.l and 215) OR 2;
	if ptemp.l>0 then af.l:=af.l and 191 (* reset Zf *) else af.l:=af.l or 64;(* set Zf *)
	if ptemp.h=0 then af.l:=af.l and 254 (* reset Cf *) else af.l:=af.l or 1;(* set Cf *)
	if (Lo XOR Mns XOR ptemp.l) and 16>0 then
        af.l:=af.l or 16 (* set Hf *)
        else
        af.l:=af.l and 239;(* reset Hf *)

	if ((Lo XOR ptemp.l) SHR 7>0) and ((Mns XOR ptemp.l) SHR 7=0) then
        af.l:=af.l or 4 (* set Pf *) else
         af.l:=af.l and 251 ;(* reset Pf *)
end;

procedure swap(var bval: Byte);
var
  h: Byte;
begin
  h := hi(bval);
  bval := (bval shl 4) or h;

  if bval = 0 then
    af.l := 64
  else
    af.l := 0;
end;

procedure anda(bval:Byte);
begin
 af.h:=af.h and bval;
 af.l:=16 or (af.h and 236) or parity[af.h];
 if af.h=0 then af.l:=af.l or 64 else af.l:=af.l and 191;
end;

procedure ora(bval:byte);
begin
 af.h:=af.h or bval;
 af.l:=(af.h and 236) or parity[af.h];
 if af.h=0 then af.l:=af.l or 64 else af.l:=af.l and 191;
end;

procedure xora(bval:byte);
begin
 af.h:=af.h xor bval;
 af.l:=(af.h and 236) or parity[af.h];
 if af.h=0 then af.l:=af.l or 64 else af.l:=af.l and 191;
end;

procedure copy_b53(btemp:Word);
Begin
 af.l:=(af.l and 215) or (btemp and 40);
end;

procedure pop(var wpval:Word);
Begin
 wpval:=wordpeek(sp_.W);
 inc(sp_.W,2);
end;

procedure push(wpval:Word);
Begin
 dec(sp_.W,2);
 wordpoke(sp_.W,wpval);
end;
