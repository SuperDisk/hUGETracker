unit LZ77;

{$mode ObjFPC}{$H+}

// The following is a translation of gitendo's capcom compression C utility
// found here: https://github.com/gitendo/gbcp/blob/master/capcom/Mega%20Man%20Xtreme%20(USA%2C%20Europe).c

// This is fixed and improved version of https://github.com/ocean1/mmx_hackpack
// Original code fails to compress cenotaph.map properly.
// Thanks to quirky nature of Capcom's tool, this one performs slightly better.
//
// tmk / https://github.com/gitendo/gbcp

interface

uses
  Classes, SysUtils;

procedure Compress(Si, So: TStream);

implementation

type
  TSEQ = record
    Offset: Integer;
    Length: Integer;
  end;

const
  MIN_LENGTH  = 3;           // minimum reference size
  MAX_LENGTH  = $7F;         // maximum literals / reference size
  WINDOW_SIZE = $FF;         // sliding window size
  FLAG_LTRLS  = 0;           // indicates literals
  FLAG_PAIR   = $80;         // indicates length,offset pair
  FLAG_EOF    = 0;           // end of compressed data

function Find(Data: PByte; Offset: Integer; Size: QWord): TSEQ;
var
  Match: TSEQ = (Offset: 0; Length: 0);
  Length, Window: Integer;
begin
  // initialize sliding window position and loop count
  if Offset < WINDOW_SIZE then
    Window := 0
  else
    Window := Offset - WINDOW_SIZE;

  // scan the window
  while Window < Offset do begin
    Length := 0;
    if Data[Window] = Data[Offset] then begin
      Inc(Length);
      while Data[Window + Length] = Data[Offset + Length] do begin
        // next byte
        Inc(Length);
        // avoid match size overflow
        if Length = MAX_LENGTH then
          Break;
        // stay in bounds
        if (window + length) > size then
          Break;
      end;
    end;
    // update if match is found and it's better then previous one
    if (Length >= MIN_LENGTH) and (Length > Match.Length) then begin
      Match.Offset := Window;
      Match.Length := Length;
    end;
    // advance to next byte in window
    Inc(Window);
  end;
  Result := Match;
end;

procedure Compress(Si, So: TStream);
var
  Size: QWord;
  Data: PByte;
  Buffer: array[0..MAX_LENGTH] of Byte;
  D: Integer = 0;
  B: Integer = 0;
  Best: TSEQ = (Offset: 0; Length: 0);
begin
  // get unpacked data size
  Size := Si.Size;

  // allocate memory and read unpacked data
  GetMem(Data, Size);
  Si.Read(Data^, Size);
  // scan unpacked data
  while D < Size do begin
    // find best match
    Best := Find(Data, D, Size);
    if Best.Length > 0 then begin
      // write literals, size and buffer content if its not empty
      if B <> 0 then begin
        So.WriteByte(FLAG_LTRLS or B);
        So.WriteBuffer(Buffer, B);
        // reset buffer offset
        B := 0;
      end;
      // write token and offset pair
      So.WriteByte(FLAG_PAIR or Best.Length);
      // stored as negative value
      So.WriteByte((Best.Offset - D) and $FF);
      // adjust data offset
      Inc(D, Best.Length);
    end else begin
      // copy one byte to buffer
      Buffer[B] := Data[D];
      Inc(B);
      Inc(D);
      // write literals if buffer is full or end of data reached
      if (B = MAX_LENGTH) or (D = Size) then begin
        So.WriteByte(FLAG_LTRLS or B);
        So.WriteBuffer(Buffer, B);
        // reset buffer offset
        B := 0;
      end;
    end;
  end;
  // mark end of compressed data
  So.WriteByte(FLAG_EOF);
  FreeMem(Data);
end;

end.

