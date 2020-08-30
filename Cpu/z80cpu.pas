{+-----------------------------------------------------------------------------+
 | Description: Gameboy Z80-CPU emulation
 |              Ported from YaSE (written by Christian Hackbart)
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit Z80CPU;

{$MODE Delphi}
{$ASMMODE Intel}

interface

{$include tables.inc}

var
  z80: array[0..511] of function: byte;

implementation

uses vars, machine;

// temporary variables
var
  addr: Word;
  qtemp: Quadruple;
  ptemp: Pair;
  w1, w2: word;
  i1, i2: Integer;

{$include procs.inc}

function nop: byte;
begin
  Result := 4;
end;

function unimplemented: byte;
begin
  Result := 0;
end;

function ld_bc_XXXX: byte;
begin
  bc.W := wordpeek(pc.W);
  Inc(pc.W, 2);

  Result := 12;
end;

function ld_mbc_a: byte;
begin
  spokeb(bc.W, af.h);

  Result := 8;
end;

function inc_bc: byte;
begin
  Inc(bc.w);

  Result := 8;
end;

function inc_b: byte;
begin
  af.l := inc_f[bc.h] or (af.l and 1);
  Inc(bc.h);

  Result := 4;
end;

function dec_b: byte;
begin
  af.l := dec_f[bc.h] or (af.l and 1);
  Dec(bc.h);

  Result := 4;
end;

function ld_b_XX: byte;
begin
  bc.h := speekb(pc.W);
  Inc(pc.W);

  Result := 8;
end;

function rlca: byte;
var
  btemp: byte;
begin
  btemp := af.l and 1;
  af.l := rlca_f[btemp][af.h] or (af.l and 196);
  af.h := rlca_a[btemp][af.h];

  Result := 4;
end;

function ld_xxxx_sp: byte;
begin
  {TODO}

  Result := 20;
end;

function add_hl_bc: byte;
begin
  add16(hl.W, bc.W);

  Result := 12;
end;

function ld_a_mbc: byte;
begin
  af.h := speekb(bc.W);

  Result := 8;
end;

function dec_bc: byte;
begin
  Dec(bc.W);

  Result := 8;
end;

function inc_c: byte;
begin
  af.l := inc_f[bc.l] or (af.l and 1);
  Inc(bc.l);

  Result := 4;
end;

function dec_c: byte;
begin
  af.l := dec_f[bc.l] or (af.l and 1);
  Dec(bc.l);

  Result := 4;
end;

function ld_c_XX: byte;
begin
  bc.l := speekb(pc.W);
  Inc(pc.W);

  Result := 8;
end;

function rrca: byte;
var
  btemp: byte;
begin
  btemp := af.l and 1;
  af.l := rrca_f[btemp][af.h] or (af.l and 196);
  af.h := rrca_a[btemp][af.h];

  Result := 4;
end;

function stop: byte;
begin
  stop_mode := 1;
  Inc(pc.w);
  Result := 10;
end;

function ld_de_XXXX: byte;
begin
  de.W := wordpeek(pc.W);
  Inc(pc.W, 2);

  Result := 12;
end;

function ld_mde_a: byte;
begin
  spokeb(de.W, af.h);

  Result := 8;
end;

function inc_de: byte;
begin
  Inc(de.W);

  Result := 8;
end;

function inc_d: byte;
begin
  af.l := inc_f[de.h] or (af.l and 1);
  Inc(de.h);

  Result := 4;
end;

function dec_d: byte;
begin
  af.l := dec_f[de.h] or (af.l and 1);
  Dec(de.h);

  Result := 4;
end;

function ld_d_XX: byte;
begin
  de.h := speekb(pc.W);
  Inc(pc.W);

  Result := 8;
end;

function rla: byte;
var
  btemp: byte;
begin
  btemp := af.l and 1;
  af.l := rla_f[btemp][af.h]; // or (af.l and 196); NOTE
  af.h := rla_a[btemp][af.h];

  Result := 4;
end;

function jr_DIS: byte;
begin
  pc.W := pc.w + shortint(speekb(pc.W)) + 1;

  Result := 12;
end;

function add_hl_de: byte;
begin
  add16(hl.W, de.W);

  Result := 12;
end;

function ld_a_mde: byte;
begin
  af.h := speekb(de.W);

  Result := 8;
end;

function dec_de: byte;
begin
  Dec(de.W);

  Result := 8;
end;

function inc_e: byte;
begin
  af.l := inc_f[de.l] or (af.l and 1);
  Inc(de.l);

  Result := 4;
end;

function dec_e: byte;
begin
  af.l := dec_f[de.l] or (af.l and 1);
  Dec(de.l);

  Result := 4;
end;

function ld_e_XX: byte;
begin
  de.l := speekb(pc.W);
  Inc(pc.W);

  Result := 8;
end;

function rra: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rra_f[af.h] or (af.l and 196);
    af.h := rra_a[0][af.h];
  end
  else
  begin
    af.l := rra_f[af.h] or (af.l and 196);
    af.h := rra_a[1][af.h];
  end;

  Result := 4;
end;

function jr_nz_DIS: byte;
begin
  if af.l and 64 = 0 then
  begin
    pc.W := pc.w + shortint(speekb(pc.W)) + 1;

    Result := 12;
  end
  else
  begin
    Inc(pc.W);

    Result := 8;
  end;
end;

function ld_hl_XXXX: byte;
begin
  hl.W := wordpeek(pc.W);
  Inc(pc.W, 2);

  Result := 12;
end;

function ldi_hl_a: byte;
begin
  spokeb(hl.w, af.h);
  Inc(hl.w);

  Result := 8;
end;

function inc_hl: byte;
begin
  Inc(hl.W);

  Result := 8;
end;

function inc_h: byte;
begin
  af.l := inc_f[hl.h] or (af.l and 1);
  Inc(hl.h);

  Result := 4;
end;

function dec_h: byte;
begin
  af.l := dec_f[hl.h] or (af.l and 1);
  Dec(hl.h);

  Result := 4;
end;

function ld_h_XX: byte;
begin
  hl.h := speekb(pc.W);
  Inc(pc.W);

  Result := 8;
end;

function daa: byte;
var
  daa_select: byte;
begin
  daa_select := af.l and 19;
  if daa_select and 16 > 0 then
  begin
    af.l := daa_f[daa_select - 12][af.h];
    af.h := daa_a[daa_select - 12][af.h];
  end
  else
  begin
    af.l := daa_f[daa_select][af.h];
    af.h := daa_a[daa_select][af.h];
  end;

  Result := 4;
end;

function jr_z_DIS: byte;
begin
  if af.l and 64 > 0 then
  begin
    pc.W := pc.w + shortint(speekb(pc.W)) + 1;

    Result := 12;
  end
  else
  begin
    Inc(pc.W);

    Result := 8;
  end;
end;

function add_hl_hl: byte;
begin
  add16(hl.W, hl.W);

  Result := 12;
end;

function ldi_a_hl: byte;
begin
  af.h := speekb(hl.w);
  Inc(hl.w);

  Result := 8;
end;

function dec_hl: byte;
begin
  Dec(hl.W);

  Result := 8;
end;

function inc_l: byte;
begin
  af.l := inc_f[hl.l] or (af.l and 1);
  Inc(hl.l);

  Result := 4;
end;

function dec_l: byte;
begin
  af.l := dec_f[hl.l] or (af.l and 1);
  Dec(hl.l);

  Result := 4;
end;

function ld_l_XX: byte;
begin
  hl.l := speekb(pc.W);
  Inc(pc.W);

  Result := 8;
end;

function cpl: byte;
begin
  af.h := af.h xor 255;
  af.l := af.l or 18;
  copy_b53(af.h);

  Result := 4;
end;

function jr_nc_DIS: byte;
begin
  if af.l and 1 = 0 then
  begin
    pc.W := pc.w + shortint(speekb(pc.W)) + 1;

    Result := 12;
  end
  else
  begin
    Inc(pc.W);

    Result := 8;
  end;
end;

function ld_sp_XXXX: byte;
begin
  sp_.W := wordpeek(pc.W);
  Inc(pc.W, 2);

  Result := 12;
end;

function ldd_hl_a: byte;
begin
  spokeb(hl.w, af.h);
  Dec(hl.w);

  Result := 8;
end;

function inc_sp: byte;
begin
  Inc(sp_.W);

  Result := 8;
end;

function inc_mhl: byte;
var
  btemp: byte;
begin
  btemp := speekb(hl.W);
  af.l := inc_f[btemp] or (af.l and 1);
  spokeb(hl.W, btemp + 1);

  Result := 12;
end;

function dec_mhl: byte;
var
  btemp: byte;
begin
  btemp := speekb(hl.W);
  af.l := dec_f[btemp] or (af.l and 1);
  spokeb(hl.W, btemp - 1);

  Result := 12;
end;

function ld_mhl_XX: byte;
begin
  spokeb(hl.W, speekb(pc.W));
  Inc(pc.W);

  Result := 12;
end;

function scf: byte;
begin
  af.l := af.l and 237;
  copy_b53(af.h);
  af.l := af.l or 1;

  Result := 4;
end;

function jr_c_DIS: byte;
begin
  if af.l and 1 > 0 then
  begin
    pc.W := pc.w + shortint(speekb(pc.W)) + 1;

    Result := 12;
  end
  else
  begin
    Inc(pc.W);

    Result := 8;
  end;
end;

function add_hl_sp: byte;
begin
  add16(hl.W, sp_.W);

  Result := 12;
end;

function ldd_a_hl: byte;
begin
  af.h := speekb(hl.w);
  Dec(hl.w);

  Result := 8;
end;

function dec_sp: byte;
begin
  Dec(sp_.W);

  Result := 8;
end;

function Z80inc_a: byte;
begin
  af.l := inc_f[af.h] or (af.l and 1);
  Inc(af.h);

  Result := 4;
end;

function Z80dec_a: byte;
begin
  af.l := dec_f[af.h] or (af.l and 1);
  Dec(af.h);

  Result := 4;
end;

function ld_a_XX: byte;
begin
  af.h := speekb(pc.W);
  Inc(pc.W);

  Result := 8;
end;

function ccf: byte;
begin
  af.l := af.l xor 1;
  af.l := af.l and 253;
  copy_b53(af.h);

  Result := 4;
end;

function ld_b_b: byte;
begin
  bc.h := bc.h;

  Result := 4;
end;

function ld_b_c: byte;
begin
  bc.h := bc.l;

  Result := 4;
end;

function ld_b_d: byte;
begin
  bc.h := de.h;

  Result := 4;
end;

function ld_b_e: byte;
begin
  bc.h := de.l;

  Result := 4;
end;

function ld_b_h: byte;
begin
  bc.h := hl.h;

  Result := 4;
end;

function ld_b_l: byte;
begin
  bc.h := hl.l;

  Result := 4;
end;

function ld_b_hl: byte;
begin
  bc.h := speekb(hl.W);

  Result := 8;
end;

function ld_b_a: byte;
begin
  bc.h := af.h;

  Result := 4;
end;

function ld_c_b: byte;
begin
  bc.l := bc.h;

  Result := 4;
end;

function ld_c_c: byte;
begin
  bc.l := bc.l;

  Result := 4;
end;

function ld_c_d: byte;
begin
  bc.l := de.h;

  Result := 4;
end;

function ld_c_e: byte;
begin
  bc.l := de.l;

  Result := 4;
end;

function ld_c_h: byte;
begin
  bc.l := hl.h;

  Result := 4;
end;

function ld_c_l: byte;
begin
  bc.l := hl.l;

  Result := 4;
end;

function ld_c_hl: byte;
begin
  bc.l := speekb(hl.W);

  Result := 8;
end;

function ld_c_a: byte;
begin
  bc.l := af.h;

  Result := 4;
end;

function ld_d_b: byte;
begin
  de.h := bc.h;

  Result := 4;
end;

function ld_d_c: byte;
begin
  de.h := bc.l;

  Result := 4;
end;

function ld_d_d: byte;
begin
  de.h := de.h;

  Result := 4;
end;

function ld_d_e: byte;
begin
  de.h := de.l;

  Result := 4;
end;

function ld_d_h: byte;
begin
  de.h := hl.h;

  Result := 4;
end;

function ld_d_l: byte;
begin
  de.h := hl.l;

  Result := 4;
end;

function ld_d_hl: byte;
begin
  de.h := speekb(hl.W);

  Result := 8;
end;

function ld_d_a: byte;
begin
  de.h := af.h;

  Result := 4;
end;

function ld_e_b: byte;
begin
  de.l := bc.h;

  Result := 4;
end;

function ld_e_c: byte;
begin
  de.l := bc.l;

  Result := 4;
end;

function ld_e_d: byte;
begin
  de.l := de.h;

  Result := 4;
end;

function ld_e_e: byte;
begin
  de.l := de.l;

  Result := 4;
end;

function ld_e_h: byte;
begin
  de.l := hl.h;

  Result := 4;
end;

function ld_e_l: byte;
begin
  de.l := hl.l;

  Result := 4;
end;

function ld_e_hl: byte;
begin
  de.l := speekb(hl.W);

  Result := 8;
end;

function ld_e_a: byte;
begin
  de.l := af.h;

  Result := 4;
end;

function ld_h_b: byte;
begin
  hl.h := bc.h;

  Result := 4;
end;

function ld_h_c: byte;
begin
  hl.h := bc.l;

  Result := 4;
end;

function ld_h_d: byte;
begin
  hl.h := de.h;

  Result := 4;
end;

function ld_h_e: byte;
begin
  hl.h := de.l;

  Result := 4;
end;

function ld_h_h: byte;
begin
  hl.h := hl.h;

  Result := 4;
end;

function ld_h_l: byte;
begin
  hl.h := hl.l;

  Result := 4;
end;

function ld_h_hl: byte;
begin
  hl.h := speekb(hl.W);

  Result := 8;
end;

function ld_h_a: byte;
begin
  hl.h := af.h;

  Result := 4;
end;

function ld_l_b: byte;
begin
  hl.l := bc.h;

  Result := 4;
end;

function ld_l_c: byte;
begin
  hl.l := bc.l;

  Result := 4;
end;

function ld_l_d: byte;
begin
  hl.l := de.h;

  Result := 4;
end;

function ld_l_e: byte;
begin
  hl.l := de.l;

  Result := 4;
end;

function ld_l_h: byte;
begin
  hl.l := hl.h;

  Result := 4;
end;

function ld_l_l: byte;
begin
  hl.l := hl.l;

  Result := 4;
end;

function ld_l_hl: byte;
begin
  hl.l := speekb(hl.W);

  Result := 8;
end;

function ld_l_a: byte;
begin
  hl.l := af.h;

  Result := 4;
end;

function ld_hl_b: byte;
begin
  spokeb(hl.W, bc.h);

  Result := 8;
end;

function ld_hl_c: byte;
begin
  spokeb(hl.W, bc.l);

  Result := 8;
end;

function ld_hl_d: byte;
begin
  spokeb(hl.W, de.h);

  Result := 8;
end;

function ld_hl_e: byte;
begin
  spokeb(hl.W, de.l);

  Result := 8;
end;

function ld_hl_h: byte;
begin
  spokeb(hl.W, hl.h);

  Result := 8;
end;

function ld_hl_l: byte;
begin
  spokeb(hl.W, hl.l);

  Result := 8;
end;

function Z80halt: byte;
begin
  if gbr_ime then
    halt_mode := 1
  else
    halt_mode := 2;
  Result := 4;
end;

function ld_hl_a: byte;
begin
  spokeb(hl.W, af.h);

  Result := 8;
end;

function ld_a_b: byte;
begin
  af.h := bc.h;

  Result := 4;
end;

function ld_a_c: byte;
begin
  af.h := bc.l;

  Result := 4;
end;

function ld_a_d: byte;
begin
  af.h := de.h;

  Result := 4;
end;

function ld_a_e: byte;
begin
  af.h := de.l;

  Result := 4;
end;

function ld_a_h: byte;
begin
  af.h := hl.h;

  Result := 4;
end;

function ld_a_l: byte;
begin
  af.h := hl.l;

  Result := 4;
end;

function ld_a_hl: byte;
begin
  af.h := speekb(hl.W);

  Result := 8;
end;

function ld_a_a: byte;
begin
  af.h := af.h;

  Result := 4;
end;

function add_a_b: byte;
begin
  add8(af.h, bc.h);

  Result := 4;
end;

function add_a_c: byte;
begin
  add8(af.h, bc.l);

  Result := 4;
end;

function add_a_d: byte;
begin
  add8(af.h, de.h);

  Result := 4;
end;

function add_a_e: byte;
begin
  add8(af.h, de.l);

  Result := 4;
end;

function add_a_h: byte;
begin
  add8(af.h, hl.h);

  Result := 4;
end;

function add_a_l: byte;
begin
  add8(af.h, hl.l);

  Result := 4;
end;

function add_a_hl: byte;
begin
  add8(af.h, speekb(hl.W));

  Result := 8;
end;

function add_a_a: byte;
begin
  add8(af.h, af.h);

  Result := 4;
end;

function adc_a_b: byte;
begin
  adc8(af.h, bc.h);

  Result := 4;
end;

function adc_a_c: byte;
begin
  adc8(af.h, bc.l);

  Result := 4;
end;

function adc_a_d: byte;
begin
  adc8(af.h, de.h);

  Result := 4;
end;

function adc_a_e: byte;
begin
  adc8(af.h, de.l);

  Result := 4;
end;

function adc_a_h: byte;
begin
  adc8(af.h, hl.h);

  Result := 4;
end;

function adc_a_l: byte;
begin
  adc8(af.h, hl.l);

  Result := 4;
end;

function adc_a_hl: byte;
begin
  adc8(af.h, speekb(hl.W));

  Result := 8;
end;

function adc_a_a: byte;
begin
  adc8(af.h, af.h);

  Result := 4;
end;

function sub_b: byte;
begin
  sub8(af.h, bc.h);

  Result := 4;
end;

function sub_c: byte;
begin
  sub8(af.h, bc.l);

  Result := 4;
end;

function sub_d: byte;
begin
  sub8(af.h, de.h);

  Result := 4;
end;

function sub_e: byte;
begin
  sub8(af.h, de.l);

  Result := 4;
end;

function sub_h: byte;
begin
  sub8(af.h, hl.h);

  Result := 4;
end;

function sub_l: byte;
begin
  sub8(af.h, hl.l);

  Result := 4;
end;

function sub_hl: byte;
begin
  sub8(af.h, speekb(hl.W));

  Result := 8;
end;

function sub_a: byte;
begin
  sub8(af.h, af.h);

  Result := 4;
end;

function sbc_a_b: byte;
begin
  sbc8(af.h, bc.h);

  Result := 4;
end;

function sbc_a_c: byte;
begin
  sbc8(af.h, bc.l);

  Result := 4;
end;

function sbc_a_d: byte;
begin
  sbc8(af.h, de.h);

  Result := 4;
end;

function sbc_a_e: byte;
begin
  sbc8(af.h, de.l);

  Result := 4;
end;

function sbc_a_h: byte;
begin
  sbc8(af.h, hl.h);

  Result := 4;
end;

function sbc_a_l: byte;
begin
  sbc8(af.h, hl.l);

  Result := 4;
end;

function sbc_a_hl: byte;
begin
  sbc8(af.h, speekb(hl.W));

  Result := 8;
end;

function sbc_a_a: byte;
begin
  sbc8(af.h, af.h);

  Result := 4;
end;

function and_b: byte;
begin
  anda(bc.h);

  Result := 4;
end;

function and_c: byte;
begin
  anda(bc.l);

  Result := 4;
end;

function and_d: byte;
begin
  anda(de.h);

  Result := 4;
end;

function and_e: byte;
begin
  anda(de.l);

  Result := 4;
end;

function and_h: byte;
begin
  anda(hl.h);

  Result := 4;
end;

function and_l: byte;
begin
  anda(hl.l);

  Result := 4;
end;

function and_hl: byte;
begin
  anda(speekb(hl.W));

  Result := 8;
end;

function and_a: byte;
begin
  anda(af.h);

  Result := 4;
end;

function xor_b: byte;
begin
  xora(bc.h);

  Result := 4;
end;

function xor_c: byte;
begin
  xora(bc.l);

  Result := 4;
end;

function xor_d: byte;
begin
  xora(de.h);

  Result := 4;
end;

function xor_e: byte;
begin
  xora(de.l);

  Result := 4;
end;

function xor_h: byte;
begin
  xora(hl.h);

  Result := 4;
end;

function xor_l: byte;
begin
  xora(hl.l);

  Result := 4;
end;

function xor_hl: byte;
begin
  xora(speekb(hl.W));

  Result := 8;
end;

function xor_a: byte;
begin
  xora(af.h);

  Result := 4;
end;

function or_b: byte;
begin
  ora(bc.h);

  Result := 4;
end;

function or_c: byte;
begin
  ora(bc.l);

  Result := 4;
end;

function or_d: byte;
begin
  ora(de.h);

  Result := 4;
end;

function or_e: byte;
begin
  ora(de.l);

  Result := 4;
end;

function or_h: byte;
begin
  ora(hl.h);

  Result := 4;
end;

function or_l: byte;
begin
  ora(hl.l);

  Result := 4;
end;

function or_hl: byte;
begin
  ora(speekb(hl.W));

  Result := 8;
end;

function or_a: byte;
begin
  ora(af.h);

  Result := 4;
end;

function cp_b: byte;
begin
  cp_sub8(af.h, bc.h);

  Result := 4;
end;

function cp_c: byte;
begin
  cp_sub8(af.h, bc.l);

  Result := 4;
end;

function cp_d: byte;
begin
  cp_sub8(af.h, de.h);

  Result := 4;
end;

function cp_e: byte;
begin
  cp_sub8(af.h, de.l);

  Result := 4;
end;

function cp_h: byte;
begin
  cp_sub8(af.h, hl.h);

  Result := 4;
end;

function cp_l: byte;
begin
  cp_sub8(af.h, hl.l);

  Result := 4;
end;

function cp_hl: byte;
begin
  cp_sub8(af.h, speekb(hl.W));

  Result := 8;
end;

function cp_a: byte;
begin
  cp_sub8(af.h, af.h);

  Result := 4;
end;

function ret_nz: byte;
begin
  if af.l and 64 = 0 then
  begin
    pop(pc.W);

    Result := 12;
  end
  else

    Result := 8;
end;

function pop_bc: byte;
begin
  pop(bc.W);

  Result := 12;
end;

function jp_nz_XXXX: byte;
begin
  if af.l and 64 = 0 then
  begin
    pc.W := wordpeek(pc.W);

    Result := 12;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function jp_XXXX: byte;
begin
  pc.W := wordpeek(pc.W);

  Result := 12;
end;

function call_nz_XXXX: byte;
begin
  if af.l and 64 = 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function push_bc: byte;
begin
  push(bc.W);

  Result := 12;
end;

function add_a_XX: byte;
begin
  add8(af.h, speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_0: byte;
begin
  push(pc.W);
  pc.W := 0;

  Result := 12;
end;

function ret_z: byte;
begin
  if af.l and 64 > 0 then
  begin
    pop(pc.W);

    Result := 12;
  end
  else

    Result := 8;
end;

function ret: byte;
begin
  pop(pc.W);

  Result := 12;
end;

function jp_z_XXXX: byte;
begin
  if af.l and 64 > 0 then
  begin
    pc.W := wordpeek(pc.W);

    Result := 12;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function PFX_CB: byte;
var
  b: byte;
begin
  b := speekb(pc.W - 1);
  addr := hl.w;

  Result := Z80[b or 256];
end;

function call_z_XXXX: byte;
begin
  if af.l and 64 > 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function call_XXXX: byte;
begin
  push(pc.W + 2);
  pc.W := wordpeek(pc.W);

  Result := 20;
end;

function adc_a_XX: byte;
begin
  adc8(af.h, speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_8: byte;
begin
  push(pc.W);
  pc.W := 8;

  Result := 12;
end;

function ret_nc: byte;
begin
  if af.l and 1 = 0 then
  begin
    pop(pc.W);

    Result := 12;
  end
  else

    Result := 8;
end;

function pop_de: byte;
begin
  pop(de.W);

  Result := 12;
end;

function jp_nc_XXXX: byte;
begin
  if af.l and 1 = 0 then
  begin
    pc.W := wordpeek(pc.W);

    Result := 12;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function call_nc_XXXX: byte;
begin
  if af.l and 1 = 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function push_de: byte;
begin
  push(de.W);

  Result := 12;
end;

function sub_XX: byte;
begin
  sub8(af.h, speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_16: byte;
begin
  push(pc.W);
  pc.W := 16;

  Result := 12;
end;

function ret_c: byte;
begin
  if af.l and 1 > 0 then
  begin
    pop(pc.W);

    Result := 12;
  end
  else

    Result := 8;
end;

function reti: byte;
begin
  w1 := speekb(sp_.w);
  Inc(sp_.w);
  w2 := speekb(sp_.w);
  Inc(sp_.w);
  pc.w := (w2 * 256) + w1;

  ei_fix := 2;
  Result := 8;
end;

function jp_c_XXXX: byte;
begin
  if af.l and 1 > 0 then
  begin
    pc.W := wordpeek(pc.W);

    Result := 12;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function call_c_XXXX: byte;
begin
  if af.l and 1 > 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function dd_debug: byte;
begin
  writeln('DEBUG: ', speekb(pc.w));
  //readln;
  Inc(pc.w);
  Result := 0;
end;

function sbc_a_XX: byte;
begin
  sbc8(af.h, speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_24: byte;
begin
  push(pc.W);
  pc.W := 24;

  Result := 12;
end;

function LDFF00plusN_A: byte;
begin
  spokeb($ff00 + speekb(pc.w), af.h);
  Inc(pc.w);

  Result := 12;
end;

function pop_hl: byte;
begin
  pop(hl.W);

  Result := 12;
end;

function ld_ff00plusC_a: byte;
begin
  spokeb($ff00 + bc.l, af.h);

  Result := 8
end;

function ex_msp_hl: byte;
var
  temp: word;
begin
  temp := wordpeek(sp_.W);
  wordpoke(sp_.W, hl.W);
  hl.W := temp;

  Result := 4;
end;

function call_po_XXXX: byte;
begin
  if af.l and 4 = 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function push_hl: byte;
begin
  push(hl.W);

  Result := 12;
end;

function and_XX: byte;
begin
  anda(speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_32: byte;
begin
  push(pc.W);
  pc.W := 32;

  Result := 12;
end;

function add_sp_dd: byte;
begin
  {TODO}
  Result := 16;
end;

function jp_hl: byte;
begin
  pc.W := hl.W;

  Result := 4;
end;

function ld_nn_a: byte;
begin
  spokeb(wordpeek(pc.w), af.h);
  Inc(pc.w, 2);

  Result := 16;
end;

function ex_de_hl: byte;
var
  temp: word;
begin
  temp := de.W;
  de.W := hl.W;
  hl.W := temp;

  Result := 4;
end;

function call_pe_XXXX: byte;
begin
  if af.l and 4 > 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function xor_XX: byte;
begin
  xora(speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_40: byte;
begin
  push(pc.W);
  pc.W := 40;

  Result := 12;
end;

function ld_a_ff00plusn: byte;
begin
  af.h := speekb($ff00 + speekb(pc.w));
  Inc(pc.w);

  Result := 12;
end;

function pop_af: byte;
begin
  pop(af.W);

  Result := 12;
end;

function ld_a_ff00plusc: byte;
begin
  af.h := speekb($ff00 + speekb(bc.l));

  Result := 8;
end;

function di: byte;
begin
  ei_fix := 0;
  di_fix := 2;
  Result := 4;
end;

function call_p_XXXX: byte;
begin
  if af.l and 128 = 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function push_af: byte;
begin
  push(af.W);

  Result := 12;
end;

function or_XX: byte;
begin
  ora(speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_48: byte;
begin
  push(pc.W);
  pc.W := 48;

  Result := 12;
end;

function ld_hl_sp_plus_dd: byte;
begin
  {TODO}
  Result := 12;
end;

function ld_sp_hl: byte;
begin
  sp_.W := hl.W;

  Result := 8;
end;

function ld_a_nn: byte;
begin
  w1 := wordpeek(pc.w);
  af.h := speekb(w1);
  Inc(pc.w, 2);

  Result := 16;
end;

function ei: byte;
begin
  di_fix := 0;
  ei_fix := 2;
  Result := 4;
end;

function call_m_XXXX: byte;
begin
  if af.l and 128 > 0 then
  begin
    push(pc.W + 2);
    pc.W := wordpeek(pc.W);

    Result := 20;
  end
  else
  begin
    Inc(pc.W, 2);

    Result := 12;
  end;
end;

function cp_XX: byte;
begin
  cp_sub8(af.h, speekb(pc.W));
  Inc(pc.W);

  Result := 8;
end;

function rst_56: byte;
begin
  push(pc.W);
  pc.W := 56;

  Result := 12;
end;

function PFX_FD: byte;
begin
  if Assigned(@FDCallback) then FDCallback;
  Result := 0;
end;

function rlc_b: byte;
begin
  af.l := rlcr_f[bc.h];
  bc.h := rlcr_a[bc.h];

  Result := 8;
end;

function rlc_c: byte;
begin
  af.l := rlcr_f[bc.l];
  bc.l := rlcr_a[bc.l];

  Result := 8;
end;

function rlc_d: byte;
begin
  af.l := rlcr_f[de.h];
  de.h := rlcr_a[de.h];

  Result := 8;
end;

function rlc_e: byte;
begin
  af.l := rlcr_f[de.l];
  de.l := rlcr_a[de.l];

  Result := 8;
end;

function rlc_h: byte;
begin
  af.l := rlcr_f[hl.h];
  hl.h := rlcr_a[hl.h];

  Result := 8;
end;

function rlc_l: byte;
begin
  af.l := rlcr_f[hl.l];
  hl.l := rlcr_a[hl.l];

  Result := 8;
end;

function rlc: byte;
var
  btemp: byte;
begin
  btemp := speekb(ADDR);
  af.l := rlcr_f[btemp];
  spokeb(ADDR, rlcr_a[btemp]);

  Result := 24;
end;

function rlc_a: byte;
begin
  af.l := rlcr_f[af.h];
  af.h := rlcr_a[af.h];

  Result := 8;
end;

function rrc_b: byte;
begin
  af.l := rrcr_f[bc.h];
  bc.h := rrcr_a[bc.h];

  Result := 8;
end;

function rrc_c: byte;
begin
  af.l := rrcr_f[bc.l];
  bc.l := rrcr_a[bc.l];

  Result := 8;
end;

function rrc_d: byte;
begin
  af.l := rrcr_f[de.h];
  de.h := rrcr_a[de.h];

  Result := 8;
end;

function rrc_e: byte;
begin
  af.l := rrcr_f[de.l];
  de.l := rrcr_a[de.l];

  Result := 8;
end;

function rrc_h: byte;
begin
  af.l := rrcr_f[hl.h];
  hl.h := rrcr_a[hl.h];

  Result := 8;
end;

function rrc_l: byte;
begin
  af.l := rrcr_f[hl.l];
  hl.l := rrcr_a[hl.l];

  Result := 8;
end;

function rrc_a: byte;
begin
  af.l := rrcr_f[af.h];
  af.h := rrcr_a[af.h];

  Result := 8;
end;

function rrc: byte;
var
  btemp: byte;
begin
  btemp := speekb(ADDR);
  af.l := rrcr_f[btemp];
  spokeb(ADDR, rrcr_a[btemp]);

  Result := 16;
end;

function rl_b: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rlr_f[0][bc.h];
    bc.h := rlr_a[0][bc.h];
  end
  else
  begin
    af.l := rlr_f[1][bc.h];
    bc.h := rlr_a[1][bc.h];
  end;

  Result := 8;
end;

function rl_c: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rlr_f[0][bc.l];
    bc.l := rlr_a[0][bc.l];
  end
  else
  begin
    af.l := rlr_f[1][bc.l];
    bc.l := rlr_a[1][bc.l];
  end;

  Result := 8;
end;

function rl_d: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rlr_f[0][de.h];
    de.h := rlr_a[0][de.h];
  end
  else
  begin
    af.l := rlr_f[1][de.h];
    de.h := rlr_a[1][de.h];
  end;

  Result := 8;
end;

function rl_e: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rlr_f[0][de.l];
    de.l := rlr_a[0][de.l];
  end
  else
  begin
    af.l := rlr_f[1][de.l];
    de.l := rlr_a[1][de.l];
  end;

  Result := 8;
end;

function rl_h: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rlr_f[0][hl.h];
    hl.h := rlr_a[0][hl.h];
  end
  else
  begin
    af.l := rlr_f[1][hl.h];
    hl.h := rlr_a[1][hl.h];
  end;

  Result := 8;
end;

function rl_l: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rlr_f[0][hl.l];
    hl.l := rlr_a[0][hl.l];
  end
  else
  begin
    af.l := rlr_f[1][hl.l];
    hl.l := rlr_a[1][hl.l];
  end;

  Result := 8;
end;

function rl: byte;
var
  btemp: byte;
begin
  if af.l and 1 = 0 then
  begin
    btemp := speekb(Addr);
    af.l := rlr_f[0][btemp];
    spokeb(Addr, rlr_a[0][btemp]);
  end
  else
  begin
    btemp := speekb(Addr);
    af.l := rlr_f[1][btemp];
    spokeb(Addr, rlr_a[1][btemp]);
  end;

  Result := 16;
end;

function rl_a: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rlr_f[0][af.h];
    af.h := rlr_a[0][af.h];
  end
  else
  begin
    af.l := rlr_f[1][af.h];
    af.h := rlr_a[1][af.h];
  end;

  Result := 8;
end;

function rr_b: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rrr_f[0][bc.h];
    bc.h := rrr_a[0][bc.h];
  end
  else
  begin
    af.l := rrr_f[1][bc.h];
    bc.h := rrr_a[1][bc.h];
  end;

  Result := 8;
end;

function rr_c: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rrr_f[0][bc.l];
    bc.l := rrr_a[0][bc.l];
  end
  else
  begin
    af.l := rrr_f[1][bc.l];
    bc.l := rrr_a[1][bc.l];
  end;

  Result := 8;
end;

function rr_d: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rrr_f[0][de.h];
    de.h := rrr_a[0][de.h];
  end
  else
  begin
    af.l := rrr_f[1][de.h];
    de.h := rrr_a[1][de.h];
  end;

  Result := 8;
end;

function rr_e: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rrr_f[0][de.l];
    de.l := rrr_a[0][de.l];
  end
  else
  begin
    af.l := rrr_f[1][de.l];
    de.l := rrr_a[1][de.l];
  end;

  Result := 8;
end;

function rr_h: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rrr_f[0][hl.h];
    hl.h := rrr_a[0][hl.h];
  end
  else
  begin
    af.l := rrr_f[1][hl.h];
    hl.h := rrr_a[1][hl.h];
  end;

  Result := 8;
end;

function rr_l: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rrr_f[0][hl.l];
    hl.l := rrr_a[0][hl.l];
  end
  else
  begin
    af.l := rrr_f[1][hl.l];
    hl.l := rrr_a[1][hl.l];
  end;

  Result := 8;
end;

function rr: byte;
var
  btemp: byte;
begin
  if af.l and 1 = 0 then
  begin
    btemp := speekb(Addr);
    af.l := rrr_f[0][btemp];
    spokeb(Addr, rrr_a[0][btemp]);
  end
  else
  begin
    btemp := speekb(Addr);
    af.l := rrr_f[1][btemp];
    spokeb(Addr, rrr_a[1][btemp]);
  end;

  Result := 16;
end;

function rr_a: byte;
begin
  if af.l and 1 = 0 then
  begin
    af.l := rrr_f[0][af.h];
    af.h := rrr_a[0][af.h];
  end
  else
  begin
    af.l := rrr_f[1][af.h];
    af.h := rrr_a[1][af.h];
  end;

  Result := 8;
end;

function sla_b: byte;
begin
  af.l := sla_f[bc.h];
  bc.h := sla_a[bc.h];

  Result := 8;
end;

function sla_c: byte;
begin
  af.l := sla_f[bc.l];
  bc.l := sla_a[bc.l];

  Result := 8;
end;

function sla_d: byte;
begin
  af.l := sla_f[de.h];
  de.h := sla_a[de.h];

  Result := 8;
end;

function sla_e: byte;
begin
  af.l := sla_f[de.l];
  de.l := sla_a[de.l];

  Result := 8;
end;

function sla_h: byte;
begin
  af.l := sla_f[hl.h];
  hl.h := sla_a[hl.h];

  Result := 8;
end;

function sla_l: byte;
begin
  af.l := sla_f[hl.l];
  hl.l := sla_a[hl.l];

  Result := 8;
end;

function sla: byte;
var
  btemp: byte;
begin
  btemp := speekb(Addr);
  af.l := sla_f[btemp];
  spokeb(Addr, sla_a[btemp]);

  Result := 16;
end;

function CBsla_a: byte;
begin
  af.l := sla_f[af.h];
  af.h := sla_a[af.h];

  Result := 8;
end;

function sra_b: byte;
begin
  af.l := sra_f[bc.h];
  bc.h := sra_a[bc.h];

  Result := 8;
end;

function sra_c: byte;
begin
  af.l := sra_f[bc.l];
  bc.l := sra_a[bc.l];

  Result := 8;
end;

function sra_d: byte;
begin
  af.l := sra_f[de.h];
  de.h := sra_a[de.h];

  Result := 8;
end;

function sra_e: byte;
begin
  af.l := sra_f[de.l];
  de.l := sra_a[de.l];

  Result := 8;
end;

function sra_h: byte;
begin
  af.l := sra_f[hl.h];
  hl.h := sra_a[hl.h];

  Result := 8;
end;

function sra_l: byte;
begin
  af.l := sra_f[hl.l];
  hl.l := sra_a[hl.l];

  Result := 8;
end;

function sra: byte;
var
  btemp: byte;
begin
  btemp := speekb(Addr);
  af.l := sra_f[btemp];
  spokeb(Addr, sra_a[btemp]);

  Result := 16;
end;

function CBsra_a: byte;
begin
  af.l := sra_f[af.h];
  af.h := sra_a[af.h];

  Result := 8;
end;

function sll_b: byte;
begin
  swap(bc.h);

  Result := 8;
end;

function sll_c: byte;
begin
  swap(bc.l);

  Result := 8;
end;

function sll_d: byte;
begin
  swap(de.h);

  Result := 8;
end;

function sll_e: byte;
begin
  swap(de.l);

  Result := 8;
end;

function sll_h: byte;
begin
  swap(hl.h);

  Result := 8;
end;

function sll_l: byte;
begin
  swap(hl.l);

  Result := 8;
end;

function sll: byte;
var
  btemp: byte;
begin
  btemp := speekb(Addr);
  swap(btemp);
  spokeb(Addr, btemp);

  Result := 16;
end;

function CBsll_a: byte;
begin
  swap(af.h);

  Result := 8;
end;

function srl_b: byte;
begin
  af.l := srl_f[bc.h];
  bc.h := srl_a[bc.h];

  Result := 8;
end;

function srl_c: byte;
begin
  af.l := srl_f[bc.l];
  bc.l := srl_a[bc.l];

  Result := 8;
end;

function srl_d: byte;
begin
  af.l := srl_f[de.h];
  de.h := srl_a[de.h];

  Result := 8;
end;

function srl_e: byte;
begin
  af.l := srl_f[de.l];
  de.l := srl_a[de.l];

  Result := 8;
end;

function srl_h: byte;
begin
  af.l := srl_f[hl.h];
  hl.h := srl_a[hl.h];

  Result := 8;
end;

function srl_l: byte;
begin
  af.l := srl_f[hl.l];
  hl.l := srl_a[hl.l];

  Result := 8;
end;

function srl: byte;
var
  btemp: byte;
begin
  btemp := speekb(Addr);
  af.l := srl_f[btemp];
  spokeb(Addr, srl_a[btemp]);

  Result := 16;
end;

function CBsrl_a: byte;
begin
  af.l := srl_f[af.h];
  af.h := srl_a[af.h];

  Result := 8;
end;

function bit0_b: byte;
begin
  af.l := bit_f0[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit0_c: byte;
begin
  af.l := bit_f0[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit0_d: byte;
begin
  af.l := bit_f0[de.h] or (af.l and 1);

  Result := 8;
end;

function bit0_e: byte;
begin
  af.l := bit_f0[de.l] or (af.l and 1);

  Result := 8;
end;

function bit0_h: byte;
begin
  af.l := bit_f0[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit0_l: byte;
begin
  af.l := bit_f0[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit0: byte;
begin
  af.l := bit_f0[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit0_a: byte;
begin
  af.l := bit_f0[af.h] or (af.l and 1);

  Result := 8;
end;

function bit1_b: byte;
begin
  af.l := bit_f1[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit1_c: byte;
begin
  af.l := bit_f1[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit1_d: byte;
begin
  af.l := bit_f1[de.h] or (af.l and 1);

  Result := 8;
end;

function bit1_e: byte;
begin
  af.l := bit_f1[de.l] or (af.l and 1);

  Result := 8;
end;

function bit1_h: byte;
begin
  af.l := bit_f1[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit1_l: byte;
begin
  af.l := bit_f1[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit1: byte;
begin
  af.l := bit_f1[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit1_a: byte;
begin
  af.l := bit_f1[af.h] or (af.l and 1);

  Result := 8;
end;

function bit2_b: byte;
begin
  af.l := bit_f2[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit2_c: byte;
begin
  af.l := bit_f2[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit2_d: byte;
begin
  af.l := bit_f2[de.h] or (af.l and 1);

  Result := 8;
end;

function bit2_e: byte;
begin
  af.l := bit_f2[de.l] or (af.l and 1);

  Result := 8;
end;

function bit2_h: byte;
begin
  af.l := bit_f2[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit2_l: byte;
begin
  af.l := bit_f2[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit2: byte;
begin
  af.l := bit_f2[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit2_a: byte;
begin
  af.l := bit_f2[af.h] or (af.l and 1);

  Result := 8;
end;

function bit3_b: byte;
begin
  af.l := bit_f3[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit3_c: byte;
begin
  af.l := bit_f3[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit3_d: byte;
begin
  af.l := bit_f3[de.h] or (af.l and 1);

  Result := 8;
end;

function bit3_e: byte;
begin
  af.l := bit_f3[de.l] or (af.l and 1);

  Result := 8;
end;

function bit3_h: byte;
begin
  af.l := bit_f3[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit3_l: byte;
begin
  af.l := bit_f3[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit3: byte;
begin
  af.l := bit_f3[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit3_a: byte;
begin
  af.l := bit_f3[af.h] or (af.l and 1);

  Result := 8;
end;

function bit4_b: byte;
begin
  af.l := bit_f4[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit4_c: byte;
begin
  af.l := bit_f4[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit4_d: byte;
begin
  af.l := bit_f4[de.h] or (af.l and 1);

  Result := 8;
end;

function bit4_e: byte;
begin
  af.l := bit_f4[de.l] or (af.l and 1);

  Result := 8;
end;

function bit4_h: byte;
begin
  af.l := bit_f4[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit4_l: byte;
begin
  af.l := bit_f4[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit4: byte;
begin
  af.l := bit_f4[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit4_a: byte;
begin
  af.l := bit_f4[af.h] or (af.l and 1);

  Result := 8;
end;

function bit5_b: byte;
begin
  af.l := bit_f5[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit5_c: byte;
begin
  af.l := bit_f5[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit5_d: byte;
begin
  af.l := bit_f5[de.h] or (af.l and 1);

  Result := 8;
end;

function bit5_e: byte;
begin
  af.l := bit_f5[de.l] or (af.l and 1);

  Result := 8;
end;

function bit5_h: byte;
begin
  af.l := bit_f5[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit5_l: byte;
begin
  af.l := bit_f5[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit5: byte;
begin
  af.l := bit_f5[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit5_a: byte;
begin
  af.l := bit_f5[af.h] or (af.l and 1);

  Result := 8;
end;

function bit6_b: byte;
begin
  af.l := bit_f6[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit6_c: byte;
begin
  af.l := bit_f6[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit6_d: byte;
begin
  af.l := bit_f6[de.h] or (af.l and 1);

  Result := 8;
end;

function bit6_e: byte;
begin
  af.l := bit_f6[de.l] or (af.l and 1);

  Result := 8;
end;

function bit6_h: byte;
begin
  af.l := bit_f6[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit6_l: byte;
begin
  af.l := bit_f6[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit6: byte;
begin
  af.l := bit_f6[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit6_a: byte;
begin
  af.l := bit_f6[af.h] or (af.l and 1);

  Result := 8;
end;

function bit7_b: byte;
begin
  af.l := bit_f7[bc.h] or (af.l and 1);

  Result := 8;
end;

function bit7_c: byte;
begin
  af.l := bit_f7[bc.l] or (af.l and 1);

  Result := 8;
end;

function bit7_d: byte;
begin
  af.l := bit_f7[de.h] or (af.l and 1);

  Result := 8;
end;

function bit7_e: byte;
begin
  af.l := bit_f7[de.l] or (af.l and 1);

  Result := 8;
end;

function bit7_h: byte;
begin
  af.l := bit_f7[hl.h] or (af.l and 1);

  Result := 8;
end;

function bit7_l: byte;
begin
  af.l := bit_f7[hl.l] or (af.l and 1);

  Result := 8;
end;

function bit7: byte;
begin
  af.l := bit_f7[speekb(Addr)] or (af.l and 1);

  Result := 12;
end;

function bit7_a: byte;
begin
  af.l := bit_f7[af.h] or (af.l and 1);

  Result := 8;
end;

function res0_b: byte;
begin
  bc.h := bc.h and 254;

  Result := 8;
end;

function res0_c: byte;
begin
  bc.l := bc.l and 254;

  Result := 8;
end;

function res0_d: byte;
begin
  de.h := de.h and 254;

  Result := 8;
end;

function res0_e: byte;
begin
  de.l := de.l and 254;

  Result := 8;
end;

function res0_h: byte;
begin
  hl.h := hl.h and 254;

  Result := 8;
end;

function res0_l: byte;
begin
  hl.l := hl.l and 254;

  Result := 8;
end;

function res0: byte;
begin
  spokeb(Addr, speekb(Addr) and 254);

  Result := 16;
end;

function res0_a: byte;
begin
  af.h := af.h and 254;

  Result := 8;
end;

function res1_b: byte;
begin
  bc.h := bc.h and 253;

  Result := 8;
end;

function res1_c: byte;
begin
  bc.l := bc.l and 253;

  Result := 8;
end;

function res1_d: byte;
begin
  de.h := de.h and 253;

  Result := 8;
end;

function res1_e: byte;
begin
  de.l := de.l and 253;

  Result := 8;
end;

function res1_h: byte;
begin
  hl.h := hl.h and 253;

  Result := 8;
end;

function res1_l: byte;
begin
  hl.l := hl.l and 253;

  Result := 8;
end;

function res1: byte;
begin
  spokeb(Addr, speekb(Addr) and 253);

  Result := 16;
end;

function res1_a: byte;
begin
  af.h := af.h and 253;

  Result := 8;
end;

function res2_b: byte;
begin
  bc.h := bc.h and 251;

  Result := 8;
end;

function res2_c: byte;
begin
  bc.l := bc.l and 251;

  Result := 8;
end;

function res2_d: byte;
begin
  de.h := de.h and 251;

  Result := 8;
end;

function res2_e: byte;
begin
  de.l := de.l and 251;

  Result := 8;
end;

function res2_h: byte;
begin
  hl.h := hl.h and 251;

  Result := 8;
end;

function res2_l: byte;
begin
  hl.l := hl.l and 251;

  Result := 8;
end;

function res2: byte;
begin
  spokeb(Addr, speekb(Addr) and 251);

  Result := 16;
end;

function res2_a: byte;
begin
  af.h := af.h and 251;

  Result := 8;
end;

function res3_b: byte;
begin
  bc.h := bc.h and 247;

  Result := 8;
end;

function res3_c: byte;
begin
  bc.l := bc.l and 247;

  Result := 8;
end;

function res3_d: byte;
begin
  de.h := de.h and 247;

  Result := 8;
end;

function res3_e: byte;
begin
  de.l := de.l and 247;

  Result := 8;
end;

function res3_h: byte;
begin
  hl.h := hl.h and 247;

  Result := 8;
end;

function res3_l: byte;
begin
  hl.l := hl.l and 247;

  Result := 8;
end;

function res3: byte;
begin
  spokeb(Addr, speekb(Addr) and 247);

  Result := 16;
end;

function res3_a: byte;
begin
  af.h := af.h and 247;

  Result := 8;
end;

function res4_b: byte;
begin
  bc.h := bc.h and 239;

  Result := 8;
end;

function res4_c: byte;
begin
  bc.l := bc.l and 239;

  Result := 8;
end;

function res4_d: byte;
begin
  de.h := de.h and 239;

  Result := 8;
end;

function res4_e: byte;
begin
  de.l := de.l and 239;

  Result := 8;
end;

function res4_h: byte;
begin
  hl.h := hl.h and 239;

  Result := 8;
end;

function res4_l: byte;
begin
  hl.l := hl.l and 239;

  Result := 8;
end;

function res4: byte;
begin
  spokeb(Addr, speekb(Addr) and 239);

  Result := 16;
end;

function res4_a: byte;
begin
  af.h := af.h and 239;

  Result := 8;
end;

function res5_b: byte;
begin
  bc.h := bc.h and 223;

  Result := 8;
end;

function res5_c: byte;
begin
  bc.l := bc.l and 223;

  Result := 8;
end;

function res5_d: byte;
begin
  de.h := de.h and 223;

  Result := 8;
end;

function res5_e: byte;
begin
  de.l := de.l and 223;

  Result := 8;
end;

function res5_h: byte;
begin
  hl.h := hl.h and 223;

  Result := 8;
end;

function res5_l: byte;
begin
  hl.l := hl.l and 223;

  Result := 8;
end;

function res5: byte;
begin
  spokeb(Addr, speekb(Addr) and 223);

  Result := 16;
end;

function res5_a: byte;
begin
  af.h := af.h and 223;

  Result := 8;
end;

function res6_b: byte;
begin
  bc.h := bc.h and 191;

  Result := 8;
end;

function res6_c: byte;
begin
  bc.l := bc.l and 191;

  Result := 8;
end;

function res6_d: byte;
begin
  de.h := de.h and 191;

  Result := 8;
end;

function res6_e: byte;
begin
  de.l := de.l and 191;

  Result := 8;
end;

function res6_h: byte;
begin
  hl.h := hl.h and 191;

  Result := 8;
end;

function res6_l: byte;
begin
  hl.l := hl.l and 191;

  Result := 8;
end;

function res6: byte;
begin
  spokeb(Addr, speekb(Addr) and 191);

  Result := 16;
end;

function res6_a: byte;
begin
  af.h := af.h and 191;

  Result := 8;
end;

function res7_b: byte;
begin
  bc.h := bc.h and 127;

  Result := 8;
end;

function res7_c: byte;
begin
  bc.l := bc.l and 127;

  Result := 8;
end;

function res7_d: byte;
begin
  de.h := de.h and 127;

  Result := 8;
end;

function res7_e: byte;
begin
  de.l := de.l and 127;

  Result := 8;
end;

function res7_h: byte;
begin
  hl.h := hl.h and 127;

  Result := 8;
end;

function res7_l: byte;
begin
  hl.l := hl.l and 127;

  Result := 8;
end;

function res7: byte;
begin
  spokeb(Addr, speekb(Addr) and 127);

  Result := 16;
end;

function res7_a: byte;
begin
  af.h := af.h and 127;

  Result := 8;
end;

function set0_b: byte;
begin
  bc.h := bc.h or 1;

  Result := 8;
end;

function set0_c: byte;
begin
  bc.l := bc.l or 1;

  Result := 8;
end;

function set0_d: byte;
begin
  de.h := de.h or 1;

  Result := 8;
end;

function set0_e: byte;
begin
  de.l := de.l or 1;

  Result := 8;
end;

function set0_h: byte;
begin
  hl.h := hl.h or 1;

  Result := 8;
end;

function set0_l: byte;
begin
  hl.l := hl.l or 1;

  Result := 8;
end;

function set0: byte;
begin
  spokeb(Addr, speekb(Addr) or 1);

  Result := 16;
end;

function set0_a: byte;
begin
  af.h := af.h or 1;

  Result := 8;
end;

function set1_b: byte;
begin
  bc.h := bc.h or 2;

  Result := 8;
end;

function set1_c: byte;
begin
  bc.l := bc.l or 2;

  Result := 8;
end;

function set1_d: byte;
begin
  de.h := de.h or 2;

  Result := 8;
end;

function set1_e: byte;
begin
  de.l := de.l or 2;

  Result := 8;
end;

function set1_h: byte;
begin
  hl.h := hl.h or 2;

  Result := 8;
end;

function set1_l: byte;
begin
  hl.l := hl.l or 2;

  Result := 8;
end;

function set1: byte;
begin
  spokeb(Addr, speekb(Addr) or 2);

  Result := 16;
end;

function set1_a: byte;
begin
  af.h := af.h or 2;

  Result := 8;
end;

function set2_b: byte;
begin
  bc.h := bc.h or 4;

  Result := 8;
end;

function set2_c: byte;
begin
  bc.l := bc.l or 4;

  Result := 8;
end;

function set2_d: byte;
begin
  de.h := de.h or 4;

  Result := 8;
end;

function set2_e: byte;
begin
  de.l := de.l or 4;

  Result := 8;
end;

function set2_h: byte;
begin
  hl.h := hl.h or 4;

  Result := 8;
end;

function set2_l: byte;
begin
  hl.l := hl.l or 4;

  Result := 8;
end;

function set2: byte;
begin
  spokeb(Addr, speekb(Addr) or 4);

  Result := 16;
end;

function set2_a: byte;
begin
  af.h := af.h or 4;

  Result := 8;
end;

function set3_b: byte;
begin
  bc.h := bc.h or 8;

  Result := 8;
end;

function set3_c: byte;
begin
  bc.l := bc.l or 8;

  Result := 8;
end;

function set3_d: byte;
begin
  de.h := de.h or 8;

  Result := 8;
end;

function set3_e: byte;
begin
  de.l := de.l or 8;

  Result := 8;
end;

function set3_h: byte;
begin
  hl.h := hl.h or 8;

  Result := 8;
end;

function set3_l: byte;
begin
  hl.l := hl.l or 8;

  Result := 8;
end;

function set3: byte;
begin
  spokeb(Addr, speekb(Addr) or 8);

  Result := 16;
end;

function set3_a: byte;
begin
  af.h := af.h or 8;

  Result := 8;
end;

function set4_b: byte;
begin
  bc.h := bc.h or 16;

  Result := 8;
end;

function set4_c: byte;
begin
  bc.l := bc.l or 16;

  Result := 8;
end;

function set4_d: byte;
begin
  de.h := de.h or 16;

  Result := 8;
end;

function set4_e: byte;
begin
  de.l := de.l or 16;

  Result := 8;
end;

function set4_h: byte;
begin
  hl.h := hl.h or 16;

  Result := 8;
end;

function set4_l: byte;
begin
  hl.l := hl.l or 16;

  Result := 8;
end;

function set4: byte;
begin
  spokeb(Addr, speekb(Addr) or 16);

  Result := 16;
end;

function set4_a: byte;
begin
  af.h := af.h or 16;

  Result := 8;
end;

function set5_b: byte;
begin
  bc.h := bc.h or 32;

  Result := 8;
end;

function set5_c: byte;
begin
  bc.l := bc.l or 32;

  Result := 8;
end;

function set5_d: byte;
begin
  de.h := de.h or 32;

  Result := 8;
end;

function set5_e: byte;
begin
  de.l := de.l or 32;

  Result := 8;
end;

function set5_h: byte;
begin
  hl.h := hl.h or 32;

  Result := 8;
end;

function set5_l: byte;
begin
  hl.l := hl.l or 32;

  Result := 8;
end;

function set5: byte;
begin
  spokeb(Addr, speekb(Addr) or 32);

  Result := 16;
end;

function set5_a: byte;
begin
  af.h := af.h or 32;

  Result := 8;
end;

function set6_b: byte;
begin
  bc.h := bc.h or 64;

  Result := 8;
end;

function set6_c: byte;
begin
  bc.l := bc.l or 64;

  Result := 8;
end;

function set6_d: byte;
begin
  de.h := de.h or 64;

  Result := 8;
end;

function set6_e: byte;
begin
  de.l := de.l or 64;

  Result := 8;
end;

function set6_h: byte;
begin
  hl.h := hl.h or 64;

  Result := 8;
end;

function set6_l: byte;
begin
  hl.l := hl.l or 64;

  Result := 8;
end;

function set6: byte;
begin
  spokeb(Addr, speekb(Addr) or 64);

  Result := 16;
end;

function set6_a: byte;
begin
  af.h := af.h or 64;

  Result := 8;
end;

function set7_b: byte;
begin
  bc.h := bc.h or 128;

  Result := 8;
end;

function set7_c: byte;
begin
  bc.l := bc.l or 128;

  Result := 8;
end;

function set7_d: byte;
begin
  de.h := de.h or 128;

  Result := 8;
end;

function set7_e: byte;
begin
  de.l := de.l or 128;

  Result := 8;
end;

function set7_h: byte;
begin
  hl.h := hl.h or 128;

  Result := 8;
end;

function set7_l: byte;
begin
  hl.l := hl.l or 128;

  Result := 8;
end;

function set7: byte;
begin
  spokeb(Addr, speekb(Addr) or 128);

  Result := 16;
end;

function set7_a: byte;
begin
  af.h := af.h or 128;

  Result := 8;
end;

begin
  Z80[0] := nop;
  Z80[1] := ld_bc_XXXX;
  Z80[2] := ld_mbc_a;
  Z80[3] := inc_bc;
  Z80[4] := inc_b;
  Z80[5] := dec_b;
  Z80[6] := ld_b_XX;
  Z80[7] := rlca;
  Z80[8] := ld_XXXX_sp;
  Z80[9] := add_hl_bc;
  Z80[10] := ld_a_mbc;
  Z80[11] := dec_bc;
  Z80[12] := inc_c;
  Z80[13] := dec_c;
  Z80[14] := ld_c_XX;
  Z80[15] := rrca;
  Z80[16] := stop;
  Z80[17] := ld_de_XXXX;
  Z80[18] := ld_mde_a;
  Z80[19] := inc_de;
  Z80[20] := inc_d;
  Z80[21] := dec_d;
  Z80[22] := ld_d_XX;
  Z80[23] := rla;
  Z80[24] := jr_DIS;
  Z80[25] := add_hl_de;
  Z80[26] := ld_a_mde;
  Z80[27] := dec_de;
  Z80[28] := inc_e;
  Z80[29] := dec_e;
  Z80[30] := ld_e_XX;
  Z80[31] := rra;
  Z80[32] := jr_nz_DIS;
  Z80[33] := ld_hl_XXXX;
  Z80[34] := ldi_hl_a;
  Z80[35] := inc_hl;
  Z80[36] := inc_h;
  Z80[37] := dec_h;
  Z80[38] := ld_h_XX;
  Z80[39] := daa;
  Z80[40] := jr_z_DIS;
  Z80[41] := add_hl_hl;
  Z80[42] := ldi_a_hl;
  Z80[43] := dec_hl;
  Z80[44] := inc_l;
  Z80[45] := dec_l;
  Z80[46] := ld_l_XX;
  Z80[47] := cpl;
  Z80[48] := jr_nc_DIS;
  Z80[49] := ld_sp_XXXX;
  Z80[50] := ldd_hl_a;
  Z80[51] := inc_sp;
  Z80[52] := inc_mhl;
  Z80[53] := dec_mhl;
  Z80[54] := ld_mhl_XX;
  Z80[55] := scf;
  Z80[56] := jr_c_DIS;
  Z80[57] := add_hl_sp;
  Z80[58] := ldd_a_hl;
  Z80[59] := dec_sp;
  Z80[60] := Z80inc_a;
  Z80[61] := Z80dec_a;
  Z80[62] := ld_a_XX;
  Z80[63] := ccf;
  Z80[64] := ld_b_b;
  Z80[65] := ld_b_c;
  Z80[66] := ld_b_d;
  Z80[67] := ld_b_e;
  Z80[68] := ld_b_h;
  Z80[69] := ld_b_l;
  Z80[70] := ld_b_hl;
  Z80[71] := ld_b_a;
  Z80[72] := ld_c_b;
  Z80[73] := ld_c_c;
  Z80[74] := ld_c_d;
  Z80[75] := ld_c_e;
  Z80[76] := ld_c_h;
  Z80[77] := ld_c_l;
  Z80[78] := ld_c_hl;
  Z80[79] := ld_c_a;
  Z80[80] := ld_d_b;
  Z80[81] := ld_d_c;
  Z80[82] := ld_d_d;
  Z80[83] := ld_d_e;
  Z80[84] := ld_d_h;
  Z80[85] := ld_d_l;
  Z80[86] := ld_d_hl;
  Z80[87] := ld_d_a;
  Z80[88] := ld_e_b;
  Z80[89] := ld_e_c;
  Z80[90] := ld_e_d;
  Z80[91] := ld_e_e;
  Z80[92] := ld_e_h;
  Z80[93] := ld_e_l;
  Z80[94] := ld_e_hl;
  Z80[95] := ld_e_a;
  Z80[96] := ld_h_b;
  Z80[97] := ld_h_c;
  Z80[98] := ld_h_d;
  Z80[99] := ld_h_e;
  Z80[100] := ld_h_h;
  Z80[101] := ld_h_l;
  Z80[102] := ld_h_hl;
  Z80[103] := ld_h_a;
  Z80[104] := ld_l_b;
  Z80[105] := ld_l_c;
  Z80[106] := ld_l_d;
  Z80[107] := ld_l_e;
  Z80[108] := ld_l_h;
  Z80[109] := ld_l_l;
  Z80[110] := ld_l_hl;
  Z80[111] := ld_l_a;
  Z80[112] := ld_hl_b;
  Z80[113] := ld_hl_c;
  Z80[114] := ld_hl_d;
  Z80[115] := ld_hl_e;
  Z80[116] := ld_hl_h;
  Z80[117] := ld_hl_l;
  Z80[118] := Z80halt;
  Z80[119] := ld_hl_a;
  Z80[120] := ld_a_b;
  Z80[121] := ld_a_c;
  Z80[122] := ld_a_d;
  Z80[123] := ld_a_e;
  Z80[124] := ld_a_h;
  Z80[125] := ld_a_l;
  Z80[126] := ld_a_hl;
  Z80[127] := ld_a_a;
  Z80[128] := add_a_b;
  Z80[129] := add_a_c;
  Z80[130] := add_a_d;
  Z80[131] := add_a_e;
  Z80[132] := add_a_h;
  Z80[133] := add_a_l;
  Z80[134] := add_a_hl;
  Z80[135] := add_a_a;
  Z80[136] := adc_a_b;
  Z80[137] := adc_a_c;
  Z80[138] := adc_a_d;
  Z80[139] := adc_a_e;
  Z80[140] := adc_a_h;
  Z80[141] := adc_a_l;
  Z80[142] := adc_a_hl;
  Z80[143] := adc_a_a;
  Z80[144] := sub_b;
  Z80[145] := sub_c;
  Z80[146] := sub_d;
  Z80[147] := sub_e;
  Z80[148] := sub_h;
  Z80[149] := sub_l;
  Z80[150] := sub_hl;
  Z80[151] := sub_a;
  Z80[152] := sbc_a_b;
  Z80[153] := sbc_a_c;
  Z80[154] := sbc_a_d;
  Z80[155] := sbc_a_e;
  Z80[156] := sbc_a_h;
  Z80[157] := sbc_a_l;
  Z80[158] := sbc_a_hl;
  Z80[159] := sbc_a_a;
  Z80[160] := and_b;
  Z80[161] := and_c;
  Z80[162] := and_d;
  Z80[163] := and_e;
  Z80[164] := and_h;
  Z80[165] := and_l;
  Z80[166] := and_hl;
  Z80[167] := and_a;
  Z80[168] := xor_b;
  Z80[169] := xor_c;
  Z80[170] := xor_d;
  Z80[171] := xor_e;
  Z80[172] := xor_h;
  Z80[173] := xor_l;
  Z80[174] := xor_hl;
  Z80[175] := xor_a;
  Z80[176] := or_b;
  Z80[177] := or_c;
  Z80[178] := or_d;
  Z80[179] := or_e;
  Z80[180] := or_h;
  Z80[181] := or_l;
  Z80[182] := or_hl;
  Z80[183] := or_a;
  Z80[184] := cp_b;
  Z80[185] := cp_c;
  Z80[186] := cp_d;
  Z80[187] := cp_e;
  Z80[188] := cp_h;
  Z80[189] := cp_l;
  Z80[190] := cp_hl;
  Z80[191] := cp_a;
  Z80[192] := ret_nz;
  Z80[193] := pop_bc;
  Z80[194] := jp_nz_XXXX;
  Z80[195] := jp_XXXX;
  Z80[196] := call_nz_XXXX;
  Z80[197] := push_bc;
  Z80[198] := add_a_XX;
  Z80[199] := rst_0;
  Z80[200] := ret_z;
  Z80[201] := ret;
  Z80[202] := jp_z_XXXX;
  Z80[203] := PFX_CB;
  Z80[204] := call_z_XXXX;
  Z80[205] := call_XXXX;
  Z80[206] := adc_a_XX;
  Z80[207] := rst_8;
  Z80[208] := ret_nc;
  Z80[209] := pop_de;
  Z80[210] := jp_nc_XXXX;
  Z80[211] := unimplemented;
  Z80[212] := call_nc_XXXX;
  Z80[213] := push_de;
  Z80[214] := sub_XX;
  Z80[215] := rst_16;
  Z80[216] := ret_c;
  Z80[217] := reti;
  Z80[218] := jp_c_XXXX;
  Z80[219] := unimplemented;
  Z80[220] := call_c_XXXX;
  Z80[221] := dd_debug;
  Z80[222] := sbc_a_XX;
  Z80[223] := rst_24;
  Z80[224] := LDFF00plusN_A;
  Z80[225] := pop_hl;
  Z80[226] := ld_ff00plusC_a;
  Z80[227] := unimplemented;
  Z80[228] := unimplemented;
  Z80[229] := push_hl;
  Z80[230] := and_XX;
  Z80[231] := rst_32;
  Z80[232] := add_sp_dd;
  Z80[233] := jp_hl;
  Z80[234] := ld_nn_a;
  Z80[235] := unimplemented;
  Z80[236] := unimplemented;
  Z80[237] := unimplemented;
  Z80[238] := xor_XX;
  Z80[239] := rst_40;
  Z80[240] := ld_a_ff00plusn;
  Z80[241] := pop_af;
  Z80[242] := ld_a_ff00plusc;
  Z80[243] := di;
  Z80[244] := unimplemented;
  Z80[245] := push_af;
  Z80[246] := or_XX;
  Z80[247] := rst_48;
  Z80[248] := ld_hl_sp_plus_dd;
  Z80[249] := ld_sp_hl;
  Z80[250] := ld_a_nn;
  Z80[251] := ei;
  Z80[252] := unimplemented;
  Z80[253] := PFX_FD;
  Z80[254] := cp_XX;
  Z80[255] := rst_56;

  // CB
  Z80[256] := rlc_b;
  Z80[257] := rlc_c;
  Z80[258] := rlc_d;
  Z80[259] := rlc_e;
  Z80[260] := rlc_h;
  Z80[261] := rlc_l;
  Z80[262] := rlc;
  Z80[263] := rlc_a;
  Z80[264] := rrc_b;
  Z80[265] := rrc_c;
  Z80[266] := rrc_d;
  Z80[267] := rrc_e;
  Z80[268] := rrc_h;
  Z80[269] := rrc_l;
  Z80[270] := rrc;
  Z80[271] := rrc_a;
  Z80[272] := rl_b;
  Z80[273] := rl_c;
  Z80[274] := rl_d;
  Z80[275] := rl_e;
  Z80[276] := rl_h;
  Z80[277] := rl_l;
  Z80[278] := rl;
  Z80[279] := rl_a;
  Z80[280] := rr_b;
  Z80[281] := rr_c;
  Z80[282] := rr_d;
  Z80[283] := rr_e;
  Z80[284] := rr_h;
  Z80[285] := rr_l;
  Z80[286] := rr;
  Z80[287] := rr_a;
  Z80[288] := sla_b;
  Z80[289] := sla_c;
  Z80[290] := sla_d;
  Z80[291] := sla_e;
  Z80[292] := sla_h;
  Z80[293] := sla_l;
  Z80[294] := sla;
  Z80[295] := CBsla_a;
  Z80[296] := sra_b;
  Z80[297] := sra_c;
  Z80[298] := sra_d;
  Z80[299] := sra_e;
  Z80[300] := sra_h;
  Z80[301] := sra_l;
  Z80[302] := sra;
  Z80[303] := CBsra_a;
  Z80[304] := sll_b;
  Z80[305] := sll_c;
  Z80[306] := sll_d;
  Z80[307] := sll_e;
  Z80[308] := sll_h;
  Z80[309] := sll_l;
  Z80[310] := sll;
  Z80[311] := CBsll_a;
  Z80[312] := srl_b;
  Z80[313] := srl_c;
  Z80[314] := srl_d;
  Z80[315] := srl_e;
  Z80[316] := srl_h;
  Z80[317] := srl_l;
  Z80[318] := srl;
  Z80[319] := CBsrl_a;
  Z80[320] := bit0_b;
  Z80[321] := bit0_c;
  Z80[322] := bit0_d;
  Z80[323] := bit0_e;
  Z80[324] := bit0_h;
  Z80[325] := bit0_l;
  Z80[326] := bit0;
  Z80[327] := bit0_a;
  Z80[328] := bit1_b;
  Z80[329] := bit1_c;
  Z80[330] := bit1_d;
  Z80[331] := bit1_e;
  Z80[332] := bit1_h;
  Z80[333] := bit1_l;
  Z80[334] := bit1;
  Z80[335] := bit1_a;
  Z80[336] := bit2_b;
  Z80[337] := bit2_c;
  Z80[338] := bit2_d;
  Z80[339] := bit2_e;
  Z80[340] := bit2_h;
  Z80[341] := bit2_l;
  Z80[342] := bit2;
  Z80[343] := bit2_a;
  Z80[344] := bit3_b;
  Z80[345] := bit3_c;
  Z80[346] := bit3_d;
  Z80[347] := bit3_e;
  Z80[348] := bit3_h;
  Z80[349] := bit3_l;
  Z80[350] := bit3;
  Z80[351] := bit3_a;
  Z80[352] := bit4_b;
  Z80[353] := bit4_c;
  Z80[354] := bit4_d;
  Z80[355] := bit4_e;
  Z80[356] := bit4_h;
  Z80[357] := bit4_l;
  Z80[358] := bit4;
  Z80[359] := bit4_a;
  Z80[360] := bit5_b;
  Z80[361] := bit5_c;
  Z80[362] := bit5_d;
  Z80[363] := bit5_e;
  Z80[364] := bit5_h;
  Z80[365] := bit5_l;
  Z80[366] := bit5;
  Z80[367] := bit5_a;
  Z80[368] := bit6_b;
  Z80[369] := bit6_c;
  Z80[370] := bit6_d;
  Z80[371] := bit6_e;
  Z80[372] := bit6_h;
  Z80[373] := bit6_l;
  Z80[374] := bit6;
  Z80[375] := bit6_a;
  Z80[376] := bit7_b;
  Z80[377] := bit7_c;
  Z80[378] := bit7_d;
  Z80[379] := bit7_e;
  Z80[380] := bit7_h;
  Z80[381] := bit7_l;
  Z80[382] := bit7;
  Z80[383] := bit7_a;
  Z80[384] := res0_b;
  Z80[385] := res0_c;
  Z80[386] := res0_d;
  Z80[387] := res0_e;
  Z80[388] := res0_h;
  Z80[389] := res0_l;
  Z80[390] := res0;
  Z80[391] := res0_a;
  Z80[392] := res1_b;
  Z80[393] := res1_c;
  Z80[394] := res1_d;
  Z80[395] := res1_e;
  Z80[396] := res1_h;
  Z80[397] := res1_l;
  Z80[398] := res1;
  Z80[399] := res1_a;
  Z80[400] := res2_b;
  Z80[401] := res2_c;
  Z80[402] := res2_d;
  Z80[403] := res2_e;
  Z80[404] := res2_h;
  Z80[405] := res2_l;
  Z80[406] := res2;
  Z80[407] := res2_a;
  Z80[408] := res3_b;
  Z80[409] := res3_c;
  Z80[410] := res3_d;
  Z80[411] := res3_e;
  Z80[412] := res3_h;
  Z80[413] := res3_l;
  Z80[414] := res3;
  Z80[415] := res3_a;
  Z80[416] := res4_b;
  Z80[417] := res4_c;
  Z80[418] := res4_d;
  Z80[419] := res4_e;
  Z80[420] := res4_h;
  Z80[421] := res4_l;
  Z80[422] := res4;
  Z80[423] := res4_a;
  Z80[424] := res5_b;
  Z80[425] := res5_c;
  Z80[426] := res5_d;
  Z80[427] := res5_e;
  Z80[428] := res5_h;
  Z80[429] := res5_l;
  Z80[430] := res5;
  Z80[431] := res5_a;
  Z80[432] := res6_b;
  Z80[433] := res6_c;
  Z80[434] := res6_d;
  Z80[435] := res6_e;
  Z80[436] := res6_h;
  Z80[437] := res6_l;
  Z80[438] := res6;
  Z80[439] := res6_a;
  Z80[440] := res7_b;
  Z80[441] := res7_c;
  Z80[442] := res7_d;
  Z80[443] := res7_e;
  Z80[444] := res7_h;
  Z80[445] := res7_l;
  Z80[446] := res7;
  Z80[447] := res7_a;
  Z80[448] := set0_b;
  Z80[449] := set0_c;
  Z80[450] := set0_d;
  Z80[451] := set0_e;
  Z80[452] := set0_h;
  Z80[453] := set0_l;
  Z80[454] := set0;
  Z80[455] := set0_a;
  Z80[456] := set1_b;
  Z80[457] := set1_c;
  Z80[458] := set1_d;
  Z80[459] := set1_e;
  Z80[460] := set1_h;
  Z80[461] := set1_l;
  Z80[462] := set1;
  Z80[463] := set1_a;
  Z80[464] := set2_b;
  Z80[465] := set2_c;
  Z80[466] := set2_d;
  Z80[467] := set2_e;
  Z80[468] := set2_h;
  Z80[469] := set2_l;
  Z80[470] := set2;
  Z80[471] := set2_a;
  Z80[472] := set3_b;
  Z80[473] := set3_c;
  Z80[474] := set3_d;
  Z80[475] := set3_e;
  Z80[476] := set3_h;
  Z80[477] := set3_l;
  Z80[478] := set3;
  Z80[479] := set3_a;
  Z80[480] := set4_b;
  Z80[481] := set4_c;
  Z80[482] := set4_d;
  Z80[483] := set4_e;
  Z80[484] := set4_h;
  Z80[485] := set4_l;
  Z80[486] := set4;
  Z80[487] := set4_a;
  Z80[488] := set5_b;
  Z80[489] := set5_c;
  Z80[490] := set5_d;
  Z80[491] := set5_e;
  Z80[492] := set5_h;
  Z80[493] := set5_l;
  Z80[494] := set5;
  Z80[495] := set5_a;
  Z80[496] := set6_b;
  Z80[497] := set6_c;
  Z80[498] := set6_d;
  Z80[499] := set6_e;
  Z80[500] := set6_h;
  Z80[501] := set6_l;
  Z80[502] := set6;
  Z80[503] := set6_a;
  Z80[504] := set7_b;
  Z80[505] := set7_c;
  Z80[506] := set7_d;
  Z80[507] := set7_e;
  Z80[508] := set7_h;
  Z80[509] := set7_l;
  Z80[510] := set7;
  Z80[511] := set7_a;
end.
