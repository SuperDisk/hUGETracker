unit opl3;

(* Nuked OPL3
 * Copyright (C) 2013-2020 Nuke.YKT
 *
 * This file is part of Nuked OPL3.
 *
 * Nuked OPL3 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
 *
 * Nuked OPL3 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Nuked OPL3. If not, see <https://www.gnu.org/licenses/>.

 *  Nuked OPL3 emulator.
 *  Thanks:
 *      MAME Development Team(Jarek Burczynski, Tatsuyuki Satoh):
 *          Feedback and Rhythm part calculation information.
 *      forums.submarine.org.uk(carbon14, opl3):
 *          Tremolo and phase generator calculation information.
 *      OPLx decapsulated(Matthew Gambrell, Olli Niemitalo):
 *          OPL2 ROMs.
 *      siliconpr0n.org(John McMaster, digshadow):
 *          YMF262 and VRC VII decaps and die shots.
 *
 * version: 1.8
 *)

interface

const
  OPL_WRITEBUF_SIZE = 1024;
  OPL_WRITEBUF_DELAY = 2;

type

opl3_slot = record
  channel: ^opl3_channel;
  chip: ^opl3_chip;
  out_: Int16;
  fbmod: Int16;
  modulo: ^Int16;
  prout: Int16;
  eg_rout: UInt16;
  eg_out: UInt16;
  eg_inc: UInt8;
  eg_gen: UInt8;
  eg_rate: UInt8;
  eg_ksl: UInt8;
  trem: ^UInt8;
  reg_vib: UInt8;
  reg_type: UInt8;
  reg_ksr: UInt8;
  reg_mult: UInt8;
  reg_ksl: UInt8;
  reg_tl: UInt8;
  reg_ar: UInt8;
  reg_dr: UInt8;
  reg_sl: UInt8;
  reg_rr: UInt8;
  reg_wf: UInt8;
  key: UInt8;
  pg_reset: UInt32;
  pg_phase: UInt32;
  pg_phase_out: UInt16;
  slot_num: UInt8;
end;

opl3_channel = record
  slots: array [0..1] of ^opl3_slot; //*slots[2]: opl3_slot;
  pair: ^opl3_channel; //*pair: opl3_channel;
  chip: ^opl3_chip; //*chip: opl3_chip;
  out_: array [0..3] of ^Int16; //*out[4]: Int16;

  chtype: UInt8;
  f_num: UInt16;
  block: UInt8;
  fb: UInt8;
  con: UInt8;
  alg: UInt8;
  ksv: UInt8;
  cha, chb: UInt16;
  ch_num: UInt8;
end;

opl3_writebuf = record
  time: UInt64;
  reg: UInt16;
  data: UInt8;
end;

opl3_chip = record
  channel: array [0..17] of opl3_channel;
  slot: array [0..35] of opl3_slot;
  timer: UInt16;
  eg_timer: UInt64;
  eg_timerrem: UInt8;
  eg_state: UInt8;
  eg_add: UInt8;
  newm: UInt8;
  nts: UInt8;
  rhy: UInt8;
  vibpos: UInt8;
  vibshift: UInt8;
  tremolo: UInt8;
  tremolopos: UInt8;
  tremoloshift: UInt8;
  noise: UInt32;
  zeromod: Int16;
  mixbuff: array [0..1] of Int32;
  rm_hh_bit2: UInt8;
  rm_hh_bit3: UInt8;
  rm_hh_bit7: UInt8;
  rm_hh_bit8: UInt8;
  rm_tc_bit3: UInt8;
  rm_tc_bit5: UInt8;

  (* OPL3L *)
  rateratio: Int32;
  samplecnt: Int32;
  oldsamples: array [0..1] of Int16;
  samples: array [0..1] of Int16;

  writebuf_samplecnt: UInt64;
  writebuf_cur: UInt32;
  writebuf_last: UInt32;
  writebuf_lasttime: UInt64;
  writebuf: array [0..OPL_WRITEBUF_SIZE-1] of opl3_writebuf;
end;

p_opl3_chip = ^opl3_chip;


procedure OPL3_Generate(chip: p_opl3_chip; buf: PInt16); external 'opl3.dll';
procedure OPL3_GenerateResampled(chip: p_opl3_chip; buf: PInt16); external 'opl3.dll';
procedure OPL3_Reset(chip: p_opl3_chip; samplerate: UInt32); external 'opl3.dll';
procedure OPL3_WriteReg(chip: p_opl3_chip; reg: UInt16; v: UInt8); external 'opl3.dll';
procedure OPL3_WriteRegBuffered(chip: p_opl3_chip; reg: UInt16; v: UInt8); external 'opl3.dll';
procedure OPL3_GenerateStream(chip: p_opl3_chip; sndptr: PInt16; numsamples: UInt32); external 'opl3.dll';

implementation

end.
