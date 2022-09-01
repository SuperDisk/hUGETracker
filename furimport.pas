unit FURImport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ZStream, Song, Instruments, hUGEDataTypes, Constants, fgl;

function LoadSongFromFURStream(Stream: TStream): TSong;

type

  { EFURException }

  EFURException = class(Exception);

implementation

type

  { TFURStream }

  TFURStream = class(Tdecompressionstream)
    function ReadSingle: Single;
    function ReadFURString: String;
  end;

  TFURHeader = record
    FurnaceModuleFormatMagic: array[0..15] of Char;
    FormatVersion: UInt16;
    Reserved1: UInt16;
    SongInfoPointer: UInt32;
    Reserved2: UInt64;
  end;

  // Jeezus, and I thought the VGM header was big.

  TFURSongInfo = record
    InfoBlockId: UInt32; // "Info" Block Id
    SizeOfThisBlock: UInt32; // Size Of This Block
    TimeBaseOfFirstSong: UInt8; // Time Base (Of First Song)
    Speed1OfFirstSong: UInt8; // Speed 1 (Of First Song)
    Speed2OfFirstSong: UInt8; // Speed 2 (Of First Song)
    InitialArpeggioTimeOfFirstSong: UInt8; // Initial Arpeggio Time (Of First Song)
    TicksPerSecondOfFirstSong: Single; // Ticks Per Second (Of First Song)
    PatternLengthOfFirstSong: UInt16; // Pattern Length (Of First Song)
    OrdersLengthOfFirstSong: UInt16; // Orders Length (Of First Song)
    HighlightAOfFirstSong: UInt8; // Highlight A (Of First Song)
    HighlightBOfFirstSong: UInt8; // Highlight B (Of First Song)
    InstrumentCount: UInt16; // Instrument Count
    WavetableCount: UInt16; // Wavetable Count
    SampleCount: UInt16; // Sample Count
    PatternCountGlobal: UInt32; // Pattern Count (Global)
    ListOfSoundChips: array[0..31] of Byte; // List Of Sound Chips
    SoundChipVolumes: array[0..31] of Byte; // Sound Chip Volumes
    SoundChipPanning: array[0..31] of Byte; // Sound Chip Panning
    SoundChipParameters: array[0..127] of Byte; // Sound Chip Parameters
    SongName: String; // Song Name
    SongAuthor: String; // Song Author
    A4Tuning: Single; // A-4 Tuning
    LimitSlides: UInt8; // Limit Slides (>=36) Or Reserved
    LinearPitch: UInt8; // Linear Pitch (>=36) Or Reserved
    LoopModality: UInt8; // Loop Modality (>=36) Or Reserved
    ProperNoiseLayout: UInt8; // Proper Noise Layout (>=42) Or Reserved
    WaveDutyIsVolume: UInt8; // Wave Duty Is Volume (>=42) Or Reserved
    ResetMacroOnPorta: UInt8; // Reset Macro On Porta (>=45) Or Reserved
    LegacyVolumeSlides: UInt8; // Legacy Volume Slides (>=45) Or Reserved
    CompatibleArpeggio: UInt8; // Compatible Arpeggio (>=45) Or Reserved
    NoteOffResetsSlides: UInt8; // Note Off Resets Slides (>=45) Or Reserved
    TargetResetsSlides: UInt8; // Target Resets Slides (>=45) Or Reserved
    ArpeggioInhibitsPortamento: UInt8; // Arpeggio Inhibits Portamento (>=47) Or Reserved
    WackAlgorithmMacro: UInt8; // Wack Algorithm Macro (>=47) Or Reserved
    BrokenShortcutSlides: UInt8; // Broken Shortcut Slides (>=49) Or Reserved
    IgnoreDuplicateSlides: UInt8; // Ignore Duplicate Slides (>=50) Or Reserved
    StopPortamentoOnNoteOff: UInt8; // Stop Portamento On Note Off (>=62) Or Reserved
    ContinuousVibrato: UInt8; // Continuous Vibrato (>=62) Or Reserved
    BrokenDacMode: UInt8; // Broken Dac Mode (>=64) Or Reserved
    OneTickCut: UInt8; // One Tick Cut (>=65) Or Reserved
    InstrumentChangeAllowedDuringPorta: UInt8; // Instrument Change Allowed During Porta (>=66) Or Reserved
    ResetNoteBaseOnArpeggioEffectStop0000: UInt8; // Reset Note Base On Arpeggio Effect Stop (0000) (>=69) Or Reserved
    PointersToInstruments: array of UInt32; // Pointers To Instruments
    PointersToWavetables: array of UInt32; // Pointers To Wavetables
    PointersToSamples: array of UInt32; // Pointers To Samples
    PointersToPatterns: array of UInt32; // Pointers To Patterns
    OrdersOfFirstSong: array of array of UInt8; // Orders (Of First Song)
    EffectColumnsOfFirstSong: array of UInt8; // Effect Columns (Of First Song)
    ChannelHideStatusOfFirstSong: array of UInt8; // Channel Hide Status (Of First Song)
    ChannelCollapseStatusOfFirstSong: array of UInt8; // Channel Collapse Status (Of First Song)
    ChannelNamesOfFirstSong: array of String; // Channel Names (Of First Song)
    ChannelShortNamesOfFirstSong: array of String; // Channel Short Names (Of First Song)
    SongComment: String; // Song Comment
    MasterVolume10F100: Single; // Master Volume, 1.0F=100% (>=59)
    BrokenSpeedSelection: UInt8; // Broken Speed Selection
    NoSlidesOnFirstTick: UInt8; // No Slides On First Tick (>=71) Or Reserved
    NextRowResetArpPos: UInt8; // Next Row Reset Arp Pos (>=71) Or Reserved
    IgnoreJumpAtEnd: UInt8; // Ignore Jump At End (>=71) Or Reserved
    BuggyPortamentoAfterSlide: UInt8; // Buggy Portamento After Slide (>=72) Or Reserved
    NewInsAffectsEnvelopeGameBoy: UInt8; // New Ins Affects Envelope (Game Boy) (>=72) Or Reserved
    ExtchChannelStateIsShared: UInt8; // Extch Channel State Is Shared (>=78) Or Reserved
    IgnoreDacModeChangeOutsideOfIntendedChannel: UInt8; // Ignore Dac Mode Change Outside Of Intended Channel (>=83) Or Reserved
    E1XyAndE2XyAlsoTakePriorityOverSlide00: UInt8; // E1Xy And E2Xy Also Take Priority Over Slide00 (>=83) Or Reserved
    NewSegaPcmWithMacrosAndProperVolPan: UInt8; // New Sega Pcm (With Macros And Proper Vol/Pan) (>=84) Or Reserved
    WeirdFNumBlockBasedChipPitchSlides: UInt8; // Weird F-Num/Block-Based Chip Pitch Slides (>=85) Or Reserved
    SnDutyMacroAlwaysResetsPhase: UInt8; // Sn Duty Macro Always Resets Phase (>=86) Or Reserved
    PitchMacroIsLinear: UInt8; // Pitch Macro Is Linear (>=90) Or Reserved
    PitchSlideSpeedInFullLinearPitchMode: UInt8; // Pitch Slide Speed In Full Linear Pitch Mode (>=94) Or Reserved
    OldOctaveBoundaryBehavior: UInt8; // Old Octave Boundary Behavior (>=97) Or Reserved
    DisableOpn2DacVolumeControl: UInt8; // Disable Opn2 Dac Volume Control (>=98) Or Reserved
    NewVolumeScalingStrategy: UInt8; // New Volume Scaling Strategy (>=99) Or Reserved
    VolumeMacroStillAppliesAfterEnd: UInt8; // Volume Macro Still Applies After End (>=99) Or Reserved
    BrokenOutvol: UInt8; // Broken Outvol (>=99) Or Reserved
    E1XyAndE2XyStopOnSameNote: UInt8; // E1Xy And E2Xy Stop On Same Note (>=100) Or Reserved
    BrokenInitialPositionOfPortaAfterArp: UInt8; // Broken Initial Position Of Porta After Arp (>=101) Or Reserved
    SnPeriodsUnder8AreTreatedAs1: UInt8; // Sn Periods Under 8 Are Treated As 1 (>=108) Or Reserved
    Reserved: array[0..5] of Byte; // Reserved
    VirtualTempoNumeratorOfFirstSong: UInt16; // Virtual Tempo Numerator Of First Song (>=96) Or Reserved
    VirtualTempoDenominatorOfFirstSong: UInt16; // Virtual Tempo Denominator Of First Song (>=96) Or Reserved
    FirstSubsongName: String; // First Subsong Name
    FirstSubsongComment: String; // First Subsong Comment
    NumberOfAdditionalSubsongs: UInt8; // Number Of Additional Subsongs
    Reserved2: array[0..2] of Byte; // Reserved
    PointersToSubsongData: array of UInt32; // Pointers To Subsong Data
    SystemName: String; // System Name
    AlbumCategoryGameName: String; // Album/Category/Game Name
    SongNameJapanese: String; // Song Name (Japanese)
    SongAuthorJapanese: String; // Song Author (Japanese)
    SystemNameJapanese: String; // System Name (Japanese)
    AlbumCategoryGameNameJapanese: String; // Album/Category/Game Name (Japanese)
  end;

  TFURInstrument = record
    InstBlockId: UInt32; // "Inst" Block Id
    SizeOfThisBlock: UInt32; // Size Of This Block
    FormatVersionSeeHeader: UInt16; // Format Version (See Header)
    InstrumentType: UInt8; // Instrument Type
    Reserved: UInt8; // Reserved
    InstrumentName: String; // Instrument Name
    AlgSusOnOpll: UInt8; // Alg (Sus On Opll)
    Feedback: UInt8; // Feedback
    FmsDcOnOpll: UInt8; // Fms (Dc On Opll)
    AmsDmOnOpll: UInt8; // Ams (Dm On Opll)
    OperatorCount: UInt8; // Operator Count
    OpllPreset: UInt8; // Opll Preset (>=60) Or Reserved
    Reserved2: UInt16; // Reserved
    Am: UInt8; // Am
    Ar: UInt8; // Ar
    Dr: UInt8; // Dr
    Mult: UInt8; // Mult
    Rr: UInt8; // Rr
    Sl: UInt8; // Sl
    Tl: UInt8; // Tl
    Dt2: UInt8; // Dt2
    Rs: UInt8; // Rs
    Dt: UInt8; // Dt
    D2R: UInt8; // D2R
    Ssgenv: UInt8; // Ssgenv
    DamForYmu759CompatRevOnOpz: UInt8; // Dam (For Ymu759 Compat; Rev On Opz)
    DvbForYmu759CompatFineOnOpz: UInt8; // Dvb (For Ymu759 Compat; Fine On Opz)
    EgtForYmu759CompatFixedfreqOnOpz: UInt8; // Egt (For Ymu759 Compat; Fixedfreq On Opz)
    KslEgshiftOnOpz: UInt8; // Ksl (Egshift On Opz)
    Sus: UInt8; // Sus
    Vib: UInt8; // Vib
    Ws: UInt8; // Ws
    Ksr: UInt8; // Ksr
    Reserved3: array[0..11] of Byte; // Reserved
    Volume: UInt8; // Volume
    Direction: UInt8; // Direction
    Length: UInt8; // Length
    SoundLength: UInt8; // Sound Length
    Triangle: UInt8; // Triangle
    Saw: UInt8; // Saw
    Pulse: UInt8; // Pulse
    Noise: UInt8; // Noise
    Attack: UInt8; // Attack
    Decay: UInt8; // Decay
    Sustain: UInt8; // Sustain
    Release: UInt8; // Release
    Duty: UInt16; // Duty
    RingMod: UInt8; // Ring Mod
    OscSync: UInt8; // Osc Sync
    ToFilter: UInt8; // To Filter
    InitFilter: UInt8; // Init Filter
    VolMacroIsCutoff: UInt8; // Vol Macro Is Cutoff
    Resonance: UInt8; // Resonance
    LowPass: UInt8; // Low Pass
    BandPass: UInt8; // Band Pass
    HighPass: UInt8; // High Pass
    Channel3Off: UInt8; // Channel 3 Off
    Cutoff: UInt16; // Cutoff
    DutyMacroIsAbsolute: UInt8; // Duty Macro Is Absolute
    FilterMacroIsAbsolute: UInt8; // Filter Macro Is Absolute
    InitialSample: UInt16; // Initial Sample
    Mode: UInt8; // Mode (>=82) Or Reserved
    WavetableLength1: UInt8; // Wavetable Length (-1) (>=82) Or Reserved
    Reserved4: array[0..11] of Byte; // Reserved
    VolumeMacroLength: UInt32; // Volume Macro Length
    ArpMacroLength: UInt32; // Arp Macro Length
    DutyMacroLength: UInt32; // Duty Macro Length
    WaveMacroLength: UInt32; // Wave Macro Length
    PitchMacroLength: UInt32; // Pitch Macro Length (>=17)
    Extra1MacroLength: UInt32; // Extra 1 Macro Length (>=17)
    Extra2MacroLength: UInt32; // Extra 2 Macro Length (>=17)
    Extra3MacroLength: UInt32; // Extra 3 Macro Length (>=17)
    VolumeMacroLoop: UInt32; // Volume Macro Loop
    ArpMacroLoop: UInt32; // Arp Macro Loop
    DutyMacroLoop: UInt32; // Duty Macro Loop
    WaveMacroLoop: UInt32; // Wave Macro Loop
    PitchMacroLoop: UInt32; // Pitch Macro Loop (>=17)
    Extra1MacroLoop: UInt32; // Extra 1 Macro Loop (>=17)
    Extra2MacroLoop: UInt32; // Extra 2 Macro Loop (>=17)
    Extra3MacroLoop: UInt32; // Extra 3 Macro Loop (>=17)
    ArpMacroMode112OrReserved: UInt8; // Arp Macro Mode (<112) Or Reserved
    Reserved5: UInt8; // Reserved (>=17) Or Volume Macro Height (>=15) Or Reserved
    Reserved6: UInt8; // Reserved (>=17) Or Duty Macro Height (>=15) Or Reserved
    Reserved7: UInt8; // Reserved (>=17) Or Wave Macro Height (>=15) Or Reserved
    VolumeMacro: array of UInt32; // Volume Macro
    ArpMacro: array of UInt32; // Arp Macro
    DutyMacro: array of UInt32; // Duty Macro
    WaveMacro: array of UInt32; // Wave Macro
    PitchMacro: array of UInt32; // Pitch Macro (>=17)
    Extra1Macro: array of UInt32; // Extra 1 Macro (>=17)
    Extra2Macro: array of UInt32; // Extra 2 Macro (>=17)
    Extra3Macro: array of UInt32; // Extra 3 Macro (>=17)
    AlgMacroLength: UInt32; // Alg Macro Length (>=29)
    FbMacroLength: UInt32; // Fb Macro Length (>=29)
    FmsMacroLength: UInt32; // Fms Macro Length (>=29)
    AmsMacroLength: UInt32; // Ams Macro Length (>=29)
    AlgMacroLoop: UInt32; // Alg Macro Loop (>=29)
    FbMacroLoop: UInt32; // Fb Macro Loop (>=29)
    FmsMacroLoop: UInt32; // Fms Macro Loop (>=29)
    AmsMacroLoop: UInt32; // Ams Macro Loop (>=29)
    VolumeMacroOpen: UInt8; // Volume Macro Open (>=29)
    ArpMacroOpen: UInt8; // Arp Macro Open (>=29)
    DutyMacroOpen: UInt8; // Duty Macro Open (>=29)
    WaveMacroOpen: UInt8; // Wave Macro Open (>=29)
    PitchMacroOpen: UInt8; // Pitch Macro Open (>=29)
    Extra1MacroOpen: UInt8; // Extra 1 Macro Open (>=29)
    Extra2MacroOpen: UInt8; // Extra 2 Macro Open (>=29)
    Extra3MacroOpen: UInt8; // Extra 3 Macro Open (>=29)
    AlgMacroOpen: UInt8; // Alg Macro Open (>=29)
    FbMacroOpen: UInt8; // Fb Macro Open (>=29)
    FmsMacroOpen: UInt8; // Fms Macro Open (>=29)
    AmsMacroOpen: UInt8; // Ams Macro Open (>=29)
    AlgMacro: array of UInt32; // Alg Macro (>=29)
    FbMacro: array of UInt32; // Fb Macro (>=29)
    FmsMacro: array of UInt32; // Fms Macro (>=29)
    AmsMacro: array of UInt32; // Ams Macro (>=29)
    AmMacroLength: UInt32; // Am Macro Length
    ArMacroLength: UInt32; // Ar Macro Length
    DrMacroLength: UInt32; // Dr Macro Length
    MultMacroLength: UInt32; // Mult Macro Length
    RrMacroLength: UInt32; // Rr Macro Length
    SlMacroLength: UInt32; // Sl Macro Length
    TlMacroLength: UInt32; // Tl Macro Length
    Dt2MacroLength: UInt32; // Dt2 Macro Length
    RsMacroLength: UInt32; // Rs Macro Length
    DtMacroLength: UInt32; // Dt Macro Length
    D2RMacroLength: UInt32; // D2R Macro Length
    SsgEgMacroLength: UInt32; // Ssg-Eg Macro Length
    AmMacroLoop: UInt32; // Am Macro Loop
    ArMacroLoop: UInt32; // Ar Macro Loop
    DrMacroLoop: UInt32; // Dr Macro Loop
    MultMacroLoop: UInt32; // Mult Macro Loop
    RrMacroLoop: UInt32; // Rr Macro Loop
    SlMacroLoop: UInt32; // Sl Macro Loop
    TlMacroLoop: UInt32; // Tl Macro Loop
    Dt2MacroLoop: UInt32; // Dt2 Macro Loop
    RsMacroLoop: UInt32; // Rs Macro Loop
    DtMacroLoop: UInt32; // Dt Macro Loop
    D2RMacroLoop: UInt32; // D2R Macro Loop
    SsgEgMacroLoop: UInt32; // Ssg-Eg Macro Loop
    AmMacroOpen: UInt8; // Am Macro Open
    ArMacroOpen: UInt8; // Ar Macro Open
    DrMacroOpen: UInt8; // Dr Macro Open
    MultMacroOpen: UInt8; // Mult Macro Open
    RrMacroOpen: UInt8; // Rr Macro Open
    SlMacroOpen: UInt8; // Sl Macro Open
    TlMacroOpen: UInt8; // Tl Macro Open
    Dt2MacroOpen: UInt8; // Dt2 Macro Open
    RsMacroOpen: UInt8; // Rs Macro Open
    DtMacroOpen: UInt8; // Dt Macro Open
    D2RMacroOpen: UInt8; // D2R Macro Open
    SsgEgMacroOpen: UInt8; // Ssg-Eg Macro Open
    AmMacro: array of UInt8; // Am Macro
    ArMacro: array of UInt8; // Ar Macro
    DrMacro: array of UInt8; // Dr Macro
    MultMacro: array of UInt8; // Mult Macro
    RrMacro: array of UInt8; // Rr Macro
    SlMacro: array of UInt8; // Sl Macro
    TlMacro: array of UInt8; // Tl Macro
    Dt2Macro: array of UInt8; // Dt2 Macro
    RsMacro: array of UInt8; // Rs Macro
    DtMacro: array of UInt8; // Dt Macro
    D2RMacro: array of UInt8; // D2R Macro
    SsgEgMacro: array of UInt8; // Ssg-Eg Macro
    VolumeMacroRelease: UInt32; // Volume Macro Release
    ArpMacroRelease: UInt32; // Arp Macro Release
    DutyMacroRelease: UInt32; // Duty Macro Release
    WaveMacroRelease: UInt32; // Wave Macro Release
    PitchMacroRelease: UInt32; // Pitch Macro Release
    Extra1MacroRelease: UInt32; // Extra 1 Macro Release
    Extra2MacroRelease: UInt32; // Extra 2 Macro Release
    Extra3MacroRelease: UInt32; // Extra 3 Macro Release
    AlgMacroRelease: UInt32; // Alg Macro Release
    FbMacroRelease: UInt32; // Fb Macro Release
    FmsMacroRelease: UInt32; // Fms Macro Release
    AmsMacroRelease: UInt32; // Ams Macro Release
    AmMacroRelease: UInt32; // Am Macro Release
    ArMacroRelease: UInt32; // Ar Macro Release
    DrMacroRelease: UInt32; // Dr Macro Release
    MultMacroRelease: UInt32; // Mult Macro Release
    RrMacroRelease: UInt32; // Rr Macro Release
    SlMacroRelease: UInt32; // Sl Macro Release
    TlMacroRelease: UInt32; // Tl Macro Release
    Dt2MacroRelease: UInt32; // Dt2 Macro Release
    RsMacroRelease: UInt32; // Rs Macro Release
    DtMacroRelease: UInt32; // Dt Macro Release
    D2RMacroRelease: UInt32; // D2R Macro Release
    SsgEgMacroRelease: UInt32; // Ssg-Eg Macro Release
    DamMacroLength: UInt32; // Dam Macro Length
    DvbMacroLength: UInt32; // Dvb Macro Length
    EgtMacroLength: UInt32; // Egt Macro Length
    KslMacroLength: UInt32; // Ksl Macro Length
    SusMacroLength: UInt32; // Sus Macro Length
    VibMacroLength: UInt32; // Vib Macro Length
    WsMacroLength: UInt32; // Ws Macro Length
    KsrMacroLength: UInt32; // Ksr Macro Length
    DamMacroLoop: UInt32; // Dam Macro Loop
    DvbMacroLoop: UInt32; // Dvb Macro Loop
    EgtMacroLoop: UInt32; // Egt Macro Loop
    KslMacroLoop: UInt32; // Ksl Macro Loop
    SusMacroLoop: UInt32; // Sus Macro Loop
    VibMacroLoop: UInt32; // Vib Macro Loop
    WsMacroLoop: UInt32; // Ws Macro Loop
    KsrMacroLoop: UInt32; // Ksr Macro Loop
    DamMacroRelease: UInt32; // Dam Macro Release
    DvbMacroRelease: UInt32; // Dvb Macro Release
    EgtMacroRelease: UInt32; // Egt Macro Release
    KslMacroRelease: UInt32; // Ksl Macro Release
    SusMacroRelease: UInt32; // Sus Macro Release
    VibMacroRelease: UInt32; // Vib Macro Release
    WsMacroRelease: UInt32; // Ws Macro Release
    KsrMacroRelease: UInt32; // Ksr Macro Release
    DamMacroOpen: UInt8; // Dam Macro Open
    DvbMacroOpen: UInt8; // Dvb Macro Open
    EgtMacroOpen: UInt8; // Egt Macro Open
    KslMacroOpen: UInt8; // Ksl Macro Open
    SusMacroOpen: UInt8; // Sus Macro Open
    VibMacroOpen: UInt8; // Vib Macro Open
    WsMacroOpen: UInt8; // Ws Macro Open
    KsrMacroOpen: UInt8; // Ksr Macro Open
    DamMacro: array of UInt8; // Dam Macro
    DvbMacro: array of UInt8; // Dvb Macro
    EgtMacro: array of UInt8; // Egt Macro
    KslMacro: array of UInt8; // Ksl Macro
    SusMacro: array of UInt8; // Sus Macro
    VibMacro: array of UInt8; // Vib Macro
    WsMacro: array of UInt8; // Ws Macro
    KsrMacro: array of UInt8; // Ksr Macro
    FixedFrequencyMode: UInt8; // Fixed Frequency Mode
    Reserved8: UInt8; // Reserved
    KickFrequency: UInt16; // Kick Frequency
    SnareHiHatFrequency: UInt16; // Snare/Hi-Hat Frequency
    TomTopFrequency: UInt16; // Tom/Top Frequency
    UseNoteMap: UInt8; // Use Note Map
    NoteFrequency120: array of UInt32; // Note Frequency × 120
    NoteSample120: array of UInt16; // Note Sample × 120
    InitialWaveform: UInt32; // Initial Waveform
    WavePosition: UInt8; // Wave Position
    WaveLength: UInt8; // Wave Length
    WaveMode: UInt8; // Wave Mode:
    Reserved9: UInt8; // Reserved
    LeftPanningMacroLength: UInt32; // Left Panning Macro Length
    RightPanningMacroLength: UInt32; // Right Panning Macro Length
    PhaseResetMacroLength: UInt32; // Phase Reset Macro Length
    Extra4MacroLength: UInt32; // Extra 4 Macro Length
    Extra5MacroLength: UInt32; // Extra 5 Macro Length
    Extra6MacroLength: UInt32; // Extra 6 Macro Length
    Extra7MacroLength: UInt32; // Extra 7 Macro Length
    Extra8MacroLength: UInt32; // Extra 8 Macro Length
    LeftPanningMacroLoop: UInt32; // Left Panning Macro Loop
    RightPanningMacroLoop: UInt32; // Right Panning Macro Loop
    PhaseResetMacroLoop: UInt32; // Phase Reset Macro Loop
    Extra4MacroLoop: UInt32; // Extra 4 Macro Loop
    Extra5MacroLoop: UInt32; // Extra 5 Macro Loop
    Extra6MacroLoop: UInt32; // Extra 6 Macro Loop
    Extra7MacroLoop: UInt32; // Extra 7 Macro Loop
    Extra8MacroLoop: UInt32; // Extra 8 Macro Loop
    LeftPanningMacroRelease: UInt32; // Left Panning Macro Release
    RightPanningMacroRelease: UInt32; // Right Panning Macro Release
    PhaseResetMacroRelease: UInt32; // Phase Reset Macro Release
    Extra4MacroRelease: UInt32; // Extra 4 Macro Release
    Extra5MacroRelease: UInt32; // Extra 5 Macro Release
    Extra6MacroRelease: UInt32; // Extra 6 Macro Release
    Extra7MacroRelease: UInt32; // Extra 7 Macro Release
    Extra8MacroRelease: UInt32; // Extra 8 Macro Release
    LeftPanningMacroOpen: UInt8; // Left Panning Macro Open
    RightPanningMacroOpen: UInt8; // Right Panning Macro Open
    PhaseResetMacroOpen: UInt8; // Phase Reset Macro Open
    Extra4MacroOpen: UInt8; // Extra 4 Macro Open
    Extra5MacroOpen: UInt8; // Extra 5 Macro Open
    Extra6MacroOpen: UInt8; // Extra 6 Macro Open
    Extra7MacroOpen: UInt8; // Extra 7 Macro Open
    Extra8MacroOpen: UInt8; // Extra 8 Macro Open
    LeftPanningMacro: array of UInt32; // Left Panning Macro
    RightPanningMacro: array of UInt32; // Right Panning Macro
    PhaseResetMacro: array of UInt32; // Phase Reset Macro
    Extra4Macro: array of UInt32; // Extra 4 Macro
    Extra5Macro: array of UInt32; // Extra 5 Macro
    Extra6Macro: array of UInt32; // Extra 6 Macro
    Extra7Macro: array of UInt32; // Extra 7 Macro
    Extra8Macro: array of UInt32; // Extra 8 Macro
    ModulationSpeed: UInt32; // Modulation Speed
    ModulationDepth: UInt32; // Modulation Depth
    InitModulationTableWithFirstWave: UInt8; // Init Modulation Table With First Wave
    Reserved10: array[0..2] of Byte; // Reserved
    ModulationTable: array[0..31] of Byte; // Modulation Table
    Fms2: UInt8; // Fms2
    Ams2: UInt8; // Ams2
    FirstWave: UInt32; // First Wave
    SecondWave: UInt32; // Second Wave
    RateDivider: UInt8; // Rate Divider
    Effect: UInt8; // Effect
    Enabled: UInt8; // Enabled
    Global: UInt8; // Global
    Speed1: UInt8; // Speed (+1)
    Parameter1: UInt8; // Parameter 1
    Parameter2: UInt8; // Parameter 2
    Parameter3: UInt8; // Parameter 3
    Parameter4: UInt8; // Parameter 4
    VolumeMacroMode: UInt8; // Volume Macro Mode
    DutyMacroMode: UInt8; // Duty Macro Mode
    WaveMacroMode: UInt8; // Wave Macro Mode
    PitchMacroMode: UInt8; // Pitch Macro Mode
    Extra1MacroMode: UInt8; // Extra 1 Macro Mode
    Extra2MacroMode: UInt8; // Extra 2 Macro Mode
    Extra3MacroMode: UInt8; // Extra 3 Macro Mode
    AlgMacroMode: UInt8; // Alg Macro Mode
    FbMacroMode: UInt8; // Fb Macro Mode
    FmsMacroMode: UInt8; // Fms Macro Mode
    AmsMacroMode: UInt8; // Ams Macro Mode
    LeftPanningMacroMode: UInt8; // Left Panning Macro Mode
    RightPanningMacroMode: UInt8; // Right Panning Macro Mode
    PhaseResetMacroMode: UInt8; // Phase Reset Macro Mode
    Extra4MacroMode: UInt8; // Extra 4 Macro Mode
    Extra5MacroMode: UInt8; // Extra 5 Macro Mode
    Extra6MacroMode: UInt8; // Extra 6 Macro Mode
    Extra7MacroMode: UInt8; // Extra 7 Macro Mode
    Extra8MacroMode: UInt8; // Extra 8 Macro Mode
    DonTTestGateBeforeNewNote: UInt8; // Don'T Test/Gate Before New Note
    AttackRate: UInt8; // Attack Rate
    Decay1Rate: UInt8; // Decay 1 Rate
    DecayLevel: UInt8; // Decay Level
    Decay2Rate: UInt8; // Decay 2 Rate
    ReleaseRate: UInt8; // Release Rate
    RateCorrection: UInt8; // Rate Correction
    LfoRate: UInt8; // Lfo Rate
    VibDepth: UInt8; // Vib Depth
    AmDepth: UInt8; // Am Depth
    Reserved11: array[0..22] of Byte; // Reserved
    UseSample: UInt8; // Use Sample
    SwitchRolesOfPhaseResetTimerAndFrequency: UInt8; // Switch Roles Of Phase Reset Timer And Frequency
    Length2: UInt8; // Length
    HardwareSequenceData: array of UInt8; // Hardware Sequence Data
    UseSoftwareEnvelope: UInt8; // Use Software Envelope
    AlwaysInitHardEnvOnNewNote: UInt8; // Always Init Hard Env On New Note
    FilterMode: UInt8; // Filter Mode
    K1: UInt16; // K1
    K2: UInt16; // K2
    EnvelopeCount: UInt16; // Envelope Count
    LeftVolumeRamp: UInt8; // Left Volume Ramp
    RightVolumeRamp: UInt8; // Right Volume Ramp
    K1Ramp: UInt8; // K1 Ramp
    K2Ramp: UInt8; // K2 Ramp
    K1Slow: UInt8; // K1 Slow
    K2Slow: UInt8; // K2 Slow
    UseEnvelope: UInt8; // Use Envelope
    GainMode: UInt8; // Gain Mode
    Gain: UInt8; // Gain
    Attack2: UInt8; // Attack
    Decay2: UInt8; // Decay
    Sustain2: UInt8; // Sustain
    Release2: UInt8; // Release
    VolumeMacroSpeed: UInt8; // Volume Macro Speed
    ArpMacroSpeed: UInt8; // Arp Macro Speed
    DutyMacroSpeed: UInt8; // Duty Macro Speed
    WaveMacroSpeed: UInt8; // Wave Macro Speed
    PitchMacroSpeed: UInt8; // Pitch Macro Speed
    Extra1MacroSpeed: UInt8; // Extra 1 Macro Speed
    Extra2MacroSpeed: UInt8; // Extra 2 Macro Speed
    Extra3MacroSpeed: UInt8; // Extra 3 Macro Speed
    AlgMacroSpeed: UInt8; // Alg Macro Speed
    FbMacroSpeed: UInt8; // Fb Macro Speed
    FmsMacroSpeed: UInt8; // Fms Macro Speed
    AmsMacroSpeed: UInt8; // Ams Macro Speed
    LeftPanningMacroSpeed: UInt8; // Left Panning Macro Speed
    RightPanningMacroSpeed: UInt8; // Right Panning Macro Speed
    PhaseResetMacroSpeed: UInt8; // Phase Reset Macro Speed
    Extra4MacroSpeed: UInt8; // Extra 4 Macro Speed
    Extra5MacroSpeed: UInt8; // Extra 5 Macro Speed
    Extra6MacroSpeed: UInt8; // Extra 6 Macro Speed
    Extra7MacroSpeed: UInt8; // Extra 7 Macro Speed
    Extra8MacroSpeed: UInt8; // Extra 8 Macro Speed
    VolumeMacroDelay: UInt8; // Volume Macro Delay
    ArpMacroDelay: UInt8; // Arp Macro Delay
    DutyMacroDelay: UInt8; // Duty Macro Delay
    WaveMacroDelay: UInt8; // Wave Macro Delay
    PitchMacroDelay: UInt8; // Pitch Macro Delay
    Extra1MacroDelay: UInt8; // Extra 1 Macro Delay
    Extra2MacroDelay: UInt8; // Extra 2 Macro Delay
    Extra3MacroDelay: UInt8; // Extra 3 Macro Delay
    AlgMacroDelay: UInt8; // Alg Macro Delay
    FbMacroDelay: UInt8; // Fb Macro Delay
    FmsMacroDelay: UInt8; // Fms Macro Delay
    AmsMacroDelay: UInt8; // Ams Macro Delay
    LeftPanningMacroDelay: UInt8; // Left Panning Macro Delay
    RightPanningMacroDelay: UInt8; // Right Panning Macro Delay
    PhaseResetMacroDelay: UInt8; // Phase Reset Macro Delay
    Extra4MacroDelay: UInt8; // Extra 4 Macro Delay
    Extra5MacroDelay: UInt8; // Extra 5 Macro Delay
    Extra6MacroDelay: UInt8; // Extra 6 Macro Delay
    Extra7MacroDelay: UInt8; // Extra 7 Macro Delay
    Extra8MacroDelay: UInt8; // Extra 8 Macro Delay
    AmMacroSpeed: UInt8; // Am Macro Speed
    ArMacroSpeed: UInt8; // Ar Macro Speed
    DrMacroSpeed: UInt8; // Dr Macro Speed
    MultMacroSpeed: UInt8; // Mult Macro Speed
    RrMacroSpeed: UInt8; // Rr Macro Speed
    SlMacroSpeed: UInt8; // Sl Macro Speed
    TlMacroSpeed: UInt8; // Tl Macro Speed
    Dt2MacroSpeed: UInt8; // Dt2 Macro Speed
    RsMacroSpeed: UInt8; // Rs Macro Speed
    DtMacroSpeed: UInt8; // Dt Macro Speed
    D2RMacroSpeed: UInt8; // D2R Macro Speed
    SsgEgMacroSpeed: UInt8; // Ssg-Eg Macro Speed
    DamMacroSpeed: UInt8; // Dam Macro Speed
    DvbMacroSpeed: UInt8; // Dvb Macro Speed
    EgtMacroSpeed: UInt8; // Egt Macro Speed
    KslMacroSpeed: UInt8; // Ksl Macro Speed
    SusMacroSpeed: UInt8; // Sus Macro Speed
    VibMacroSpeed: UInt8; // Vib Macro Speed
    WsMacroSpeed: UInt8; // Ws Macro Speed
    KsrMacroSpeed: UInt8; // Ksr Macro Speed
    AmMacroDelay: UInt8; // Am Macro Delay
    ArMacroDelay: UInt8; // Ar Macro Delay
    DrMacroDelay: UInt8; // Dr Macro Delay
    MultMacroDelay: UInt8; // Mult Macro Delay
    RrMacroDelay: UInt8; // Rr Macro Delay
    SlMacroDelay: UInt8; // Sl Macro Delay
    TlMacroDelay: UInt8; // Tl Macro Delay
    Dt2MacroDelay: UInt8; // Dt2 Macro Delay
    RsMacroDelay: UInt8; // Rs Macro Delay
    DtMacroDelay: UInt8; // Dt Macro Delay
    D2RMacroDelay: UInt8; // D2R Macro Delay
    SsgEgMacroDelay: UInt8; // Ssg-Eg Macro Delay
    DamMacroDelay: UInt8; // Dam Macro Delay
    DvbMacroDelay: UInt8; // Dvb Macro Delay
    EgtMacroDelay: UInt8; // Egt Macro Delay
    KslMacroDelay: UInt8; // Ksl Macro Delay
    SusMacroDelay: UInt8; // Sus Macro Delay
    VibMacroDelay: UInt8; // Vib Macro Delay
    WsMacroDelay: UInt8; // Ws Macro Delay
    KsrMacroDelay: UInt8; // Ksr Macro Delay
  end;

  TFURRow = record
    Note: Smallint;
    Octave: Smallint;
    Instrument: Smallint;
    Volume: Smallint;
    Effects: array of record
      EffectCode, EffectParams: Smallint;
    end;
  end;

  TFURPattern = record
    PatrBlockId: UInt32; // "Patr" Block Id
    SizeOfThisBlock: UInt32; // Size Of This Block
    Channel: UInt16; // Channel
    PatternIndex: UInt16; // Pattern Index
    Subsong: UInt16; // Subsong (>=95) Or Reserved
    Reserved: UInt16; // Reserved
    PatternData: array of TFURRow; // Pattern Data
    PatternName: String; // Pattern Name (>=51)
  end;

  TFURInstrumentMap = specialize TFPGMap<Integer, Integer>;

const
  ContinuousEffects = [
    $0
  ];

procedure CleanupPattern(Pat: PPattern);// InstMap: TTBMInstrumentMap);
var
  I: Integer;
  CurNote: Integer = NO_NOTE;
  ContinuousCode: Byte = 0;
  ContinuousParam: Byte = 0;
begin
  for I := Low(TPattern) to High(TPattern) do begin
    {if not InstMap.TryGetData(Pat^[I].Instrument-1, Pat^[I].Instrument) then
      Pat^[I].Instrument := 0;}

    if Pat^[I].Note <> NO_NOTE then
      CurNote := Pat^[I].Note;

    if (Pat^[I].Instrument <> 0) and (Pat^[I].Note = NO_NOTE) then
      Pat^[I].Note := CurNote;

    if Pat^[I].Volume = 1 then begin
      ContinuousCode := Pat^[I].EffectCode;
      ContinuousParam := Pat^[I].EffectParams.Value;
    end;

    if (ContinuousParam <> 0) and (Pat^[I].EffectCode = 0) and (Pat^[I].EffectParams.Value = 0) then begin
      Pat^[I].EffectCode := ContinuousCode;
      Pat^[I].EffectParams.Value := ContinuousParam;
    end;

    Pat^[I].Volume := 0;
  end;
end;

function ReadFURPattern(S: TFURStream; PatLength: Integer; NumFX: array of Byte): TFURPattern;
var
  I, J: Integer;
begin
  Result.PatrBlockId := S.ReadDWord;
  Result.SizeOfThisBlock := S.ReadDWord;
  Result.Channel := S.ReadWord;
  Result.PatternIndex := S.ReadWord;
  Result.Subsong := S.ReadWord;
  Result.Reserved := S.ReadWord;

  SetLength(Result.PatternData, PatLength);
  for I := Low(Result.PatternData) to High(Result.PatternData) do begin
    with Result.PatternData[I] do begin
      Note := Smallint(S.ReadWord);
      Octave := Smallint(S.ReadWord);
      Instrument := Smallint(S.ReadWord);
      Volume := Smallint(S.ReadWord);

      SetLength(Effects, NumFX[Result.Channel]);
      for J := Low(Effects) to High(Effects) do begin
        Effects[J].EffectCode := Smallint(S.ReadWord);
        Effects[J].EffectParams := Smallint(S.ReadWord);
      end;
    end;
  end;

  Result.PatternName := S.ReadFURString;
end;

function ReadFURHeader(S: TFURStream): TFURHeader;
begin
  S.ReadBuffer(Result.FurnaceModuleFormatMagic[0], SizeOf(Byte)*16);
  Result.FormatVersion := S.ReadWord;
  Result.Reserved1 := S.ReadWord;
  Result.SongInfoPointer := S.ReadDWord;
  Result.Reserved2 := S.ReadQWord;
end;

function ReadFURInstrument(S: TFurStream): TFURInstrument;
var
  I: Integer;
begin
  Result := Default(TFURInstrument);

  Result.InstBlockId := S.ReadDWord;
  Result.SizeOfThisBlock := S.ReadDWord;
  Result.FormatVersionSeeHeader := S.ReadWord;
  Result.InstrumentType := S.ReadByte;
  Result.Reserved := S.ReadByte;
  Result.InstrumentName := S.ReadFURString;
  Result.AlgSusOnOpll := S.ReadByte;
  Result.Feedback := S.ReadByte;
  Result.FmsDcOnOpll := S.ReadByte;
  Result.AmsDmOnOpll := S.ReadByte;
  Result.OperatorCount := S.ReadByte;
  Result.OpllPreset := S.ReadByte;
  Result.Reserved2 := S.ReadWord;
  for I := 1 to 4 do begin // --- | **FM operator data** × 4
    Result.Am := S.ReadByte;
    Result.Ar := S.ReadByte;
    Result.Dr := S.ReadByte;
    Result.Mult := S.ReadByte;
    Result.Rr := S.ReadByte;
    Result.Sl := S.ReadByte;
    Result.Tl := S.ReadByte;
    Result.Dt2 := S.ReadByte;
    Result.Rs := S.ReadByte;
    Result.Dt := S.ReadByte;
    Result.D2R := S.ReadByte;
    Result.Ssgenv := S.ReadByte;
    Result.DamForYmu759CompatRevOnOpz := S.ReadByte;
    Result.DvbForYmu759CompatFineOnOpz := S.ReadByte;
    Result.EgtForYmu759CompatFixedfreqOnOpz := S.ReadByte;
    Result.KslEgshiftOnOpz := S.ReadByte;
    Result.Sus := S.ReadByte;
    Result.Vib := S.ReadByte;
    Result.Ws := S.ReadByte;
    Result.Ksr := S.ReadByte;
    S.ReadBuffer(Result.Reserved3[0], SizeOf(Byte)*12);
  end;
  Result.Volume := S.ReadByte;
  Result.Direction := S.ReadByte;
  Result.Length := S.ReadByte;
  Result.SoundLength := S.ReadByte;
  Result.Triangle := S.ReadByte;
  Result.Saw := S.ReadByte;
  Result.Pulse := S.ReadByte;
  Result.Noise := S.ReadByte;
  Result.Attack := S.ReadByte;
  Result.Decay := S.ReadByte;
  Result.Sustain := S.ReadByte;
  Result.Release := S.ReadByte;
  Result.Duty := S.ReadWord;
  Result.RingMod := S.ReadByte;
  Result.OscSync := S.ReadByte;
  Result.ToFilter := S.ReadByte;
  Result.InitFilter := S.ReadByte;
  Result.VolMacroIsCutoff := S.ReadByte;
  Result.Resonance := S.ReadByte;
  Result.LowPass := S.ReadByte;
  Result.BandPass := S.ReadByte;
  Result.HighPass := S.ReadByte;
  Result.Channel3Off := S.ReadByte;
  Result.Cutoff := S.ReadWord;
  Result.DutyMacroIsAbsolute := S.ReadByte;
  Result.FilterMacroIsAbsolute := S.ReadByte;
  Result.InitialSample := S.ReadWord;
  Result.Mode := S.ReadByte;
  Result.WavetableLength1 := S.ReadByte;
  S.ReadBuffer(Result.Reserved4[0], SizeOf(Byte)*12);
  Result.VolumeMacroLength := S.ReadDWord;
  Result.ArpMacroLength := S.ReadDWord;
  Result.DutyMacroLength := S.ReadDWord;
  Result.WaveMacroLength := S.ReadDWord;
  Result.PitchMacroLength := S.ReadDWord;
  Result.Extra1MacroLength := S.ReadDWord;
  Result.Extra2MacroLength := S.ReadDWord;
  Result.Extra3MacroLength := S.ReadDWord;
  Result.VolumeMacroLoop := S.ReadDWord;
  Result.ArpMacroLoop := S.ReadDWord;
  Result.DutyMacroLoop := S.ReadDWord;
  Result.WaveMacroLoop := S.ReadDWord;
  Result.PitchMacroLoop := S.ReadDWord;
  Result.Extra1MacroLoop := S.ReadDWord;
  Result.Extra2MacroLoop := S.ReadDWord;
  Result.Extra3MacroLoop := S.ReadDWord;
  Result.ArpMacroMode112OrReserved := S.ReadByte;
  Result.Reserved5 := S.ReadByte;
  Result.Reserved6 := S.ReadByte;
  Result.Reserved7 := S.ReadByte;
  SetLength(Result.VolumeMacro, Result.VolumeMacroLength);
  if Length(Result.VolumeMacro) <> 0 then
    S.ReadBuffer(Result.VolumeMacro[0], SizeOf(UInt32)*Result.VolumeMacroLength);
  SetLength(Result.ArpMacro, Result.ArpMacroLength);
  if Length(Result.ArpMacro) <> 0 then
    S.ReadBuffer(Result.ArpMacro[0], SizeOf(UInt32)*Result.ArpMacroLength);
  SetLength(Result.DutyMacro, Result.DutyMacroLength);
  if Length(Result.DutyMacro) <> 0 then
    S.ReadBuffer(Result.DutyMacro[0], SizeOf(UInt32)*Result.DutyMacroLength);
  SetLength(Result.WaveMacro, Result.WaveMacroLength);
  if Length(Result.WaveMacro) <> 0 then
    S.ReadBuffer(Result.WaveMacro[0], SizeOf(UInt32)*Result.WaveMacroLength);
  SetLength(Result.PitchMacro, Result.PitchMacroLength);
  if Length(Result.PitchMacro) <> 0 then
    S.ReadBuffer(Result.PitchMacro[0], SizeOf(UInt32)*Result.PitchMacroLength);
  SetLength(Result.Extra1Macro, Result.Extra1MacroLength);
  if Length(Result.Extra1Macro) <> 0 then
    S.ReadBuffer(Result.Extra1Macro[0], SizeOf(UInt32)*Result.Extra1MacroLength);
  SetLength(Result.Extra2Macro, Result.Extra2MacroLength);
  if Length(Result.Extra2Macro) <> 0 then
    S.ReadBuffer(Result.Extra2Macro[0], SizeOf(UInt32)*Result.Extra2MacroLength);
  SetLength(Result.Extra3Macro, Result.Extra3MacroLength);
  if Length(Result.Extra3Macro) <> 0 then
    S.ReadBuffer(Result.Extra3Macro[0], SizeOf(UInt32)*Result.Extra3MacroLength);
  Result.AlgMacroLength := S.ReadDWord;
  Result.FbMacroLength := S.ReadDWord;
  Result.FmsMacroLength := S.ReadDWord;
  Result.AmsMacroLength := S.ReadDWord;
  Result.AlgMacroLoop := S.ReadDWord;
  Result.FbMacroLoop := S.ReadDWord;
  Result.FmsMacroLoop := S.ReadDWord;
  Result.AmsMacroLoop := S.ReadDWord;
  Result.VolumeMacroOpen := S.ReadByte;
  Result.ArpMacroOpen := S.ReadByte;
  Result.DutyMacroOpen := S.ReadByte;
  Result.WaveMacroOpen := S.ReadByte;
  Result.PitchMacroOpen := S.ReadByte;
  Result.Extra1MacroOpen := S.ReadByte;
  Result.Extra2MacroOpen := S.ReadByte;
  Result.Extra3MacroOpen := S.ReadByte;
  Result.AlgMacroOpen := S.ReadByte;
  Result.FbMacroOpen := S.ReadByte;
  Result.FmsMacroOpen := S.ReadByte;
  Result.AmsMacroOpen := S.ReadByte;
  SetLength(Result.AlgMacro, Result.AlgMacroLength);
  if Length(Result.AlgMacro) <> 0 then
    S.ReadBuffer(Result.AlgMacro[0], SizeOf(UInt32)*Result.AlgMacroLength);
  SetLength(Result.FbMacro, Result.FbMacroLength);
  if Length(Result.FbMacro) <> 0 then
    S.ReadBuffer(Result.FbMacro[0], SizeOf(UInt32)*Result.FbMacroLength);
  SetLength(Result.FmsMacro, Result.FmsMacroLength);
  if Length(Result.FmsMacro) <> 0 then
    S.ReadBuffer(Result.FmsMacro[0], SizeOf(UInt32)*Result.FmsMacroLength);
  SetLength(Result.AmsMacro, Result.AmsMacroLength);
  if Length(Result.AmsMacro) <> 0 then
    S.ReadBuffer(Result.AmsMacro[0], SizeOf(UInt32)*Result.AmsMacroLength);
  for I := 1 to 4 do begin // --- | **operator macro headers** × 4 (>=29)
    Result.AmMacroLength := S.ReadDWord;
    Result.ArMacroLength := S.ReadDWord;
    Result.DrMacroLength := S.ReadDWord;
    Result.MultMacroLength := S.ReadDWord;
    Result.RrMacroLength := S.ReadDWord;
    Result.SlMacroLength := S.ReadDWord;
    Result.TlMacroLength := S.ReadDWord;
    Result.Dt2MacroLength := S.ReadDWord;
    Result.RsMacroLength := S.ReadDWord;
    Result.DtMacroLength := S.ReadDWord;
    Result.D2RMacroLength := S.ReadDWord;
    Result.SsgEgMacroLength := S.ReadDWord;
    Result.AmMacroLoop := S.ReadDWord;
    Result.ArMacroLoop := S.ReadDWord;
    Result.DrMacroLoop := S.ReadDWord;
    Result.MultMacroLoop := S.ReadDWord;
    Result.RrMacroLoop := S.ReadDWord;
    Result.SlMacroLoop := S.ReadDWord;
    Result.TlMacroLoop := S.ReadDWord;
    Result.Dt2MacroLoop := S.ReadDWord;
    Result.RsMacroLoop := S.ReadDWord;
    Result.DtMacroLoop := S.ReadDWord;
    Result.D2RMacroLoop := S.ReadDWord;
    Result.SsgEgMacroLoop := S.ReadDWord;
    Result.AmMacroOpen := S.ReadByte;
    Result.ArMacroOpen := S.ReadByte;
    Result.DrMacroOpen := S.ReadByte;
    Result.MultMacroOpen := S.ReadByte;
    Result.RrMacroOpen := S.ReadByte;
    Result.SlMacroOpen := S.ReadByte;
    Result.TlMacroOpen := S.ReadByte;
    Result.Dt2MacroOpen := S.ReadByte;
    Result.RsMacroOpen := S.ReadByte;
    Result.DtMacroOpen := S.ReadByte;
    Result.D2RMacroOpen := S.ReadByte;
    Result.SsgEgMacroOpen := S.ReadByte;
  end;
  for I := 1 to 4 do begin //  --- | **operator macros** × 4 (>=29)
    SetLength(Result.AmMacro, Result.AmMacroLength);
    if Length(Result.AmMacro) <> 0 then
      S.ReadBuffer(Result.AmMacro[0], SizeOf(UInt8)*Result.AmMacroLength);
    SetLength(Result.ArMacro, Result.ArMacroLength);
    if Length(Result.ArMacro) <> 0 then
      S.ReadBuffer(Result.ArMacro[0], SizeOf(UInt8)*Result.ArMacroLength);
    SetLength(Result.DrMacro, Result.DrMacroLength);
    if Length(Result.DrMacro) <> 0 then
      S.ReadBuffer(Result.DrMacro[0], SizeOf(UInt8)*Result.DrMacroLength);
    SetLength(Result.MultMacro, Result.MultMacroLength);
    if Length(Result.MultMacro) <> 0 then
      S.ReadBuffer(Result.MultMacro[0], SizeOf(UInt8)*Result.MultMacroLength);
    SetLength(Result.RrMacro, Result.RrMacroLength);
    if Length(Result.RrMacro) <> 0 then
      S.ReadBuffer(Result.RrMacro[0], SizeOf(UInt8)*Result.RrMacroLength);
    SetLength(Result.SlMacro, Result.SlMacroLength);
    if Length(Result.SlMacro) <> 0 then
      S.ReadBuffer(Result.SlMacro[0], SizeOf(UInt8)*Result.SlMacroLength);
    SetLength(Result.TlMacro, Result.TlMacroLength);
    if Length(Result.TlMacro) <> 0 then
      S.ReadBuffer(Result.TlMacro[0], SizeOf(UInt8)*Result.TlMacroLength);
    SetLength(Result.Dt2Macro, result.Dt2MacroLength);
    if Length(Result.Dt2Macro) <> 0 then
      S.ReadBuffer(Result.Dt2Macro[0], SizeOf(UInt8)*Result.Dt2MacroLength);
    SetLength(Result.RsMacro, result.RsMacroLength);
    if Length(Result.RsMacro) <> 0 then
      S.ReadBuffer(Result.RsMacro[0], SizeOf(UInt8)*Result.RsMacroLength);
    SetLength(Result.DtMacro, Result.DtMacroLength);
    if Length(Result.DtMacro) <> 0 then
      S.ReadBuffer(Result.DtMacro[0], SizeOf(UInt8)*Result.DtMacroLength);
    SetLength(Result.D2RMacro, Result.D2RMacroLength);
    if Length(Result.D2RMacro) <> 0 then
      S.ReadBuffer(Result.D2RMacro[0], SizeOf(UInt8)*Result.D2RMacroLength);
    SetLength(Result.SsgEgMacro, Result.SsgEgMacroLength);
    if Length(Result.SsgEgMacro) <> 0 then
      S.ReadBuffer(Result.SsgEgMacro[0], SizeOf(UInt8)*Result.SsgEgMacroLength);
  end;
  Result.VolumeMacroRelease := S.ReadDWord;
  Result.ArpMacroRelease := S.ReadDWord;
  Result.DutyMacroRelease := S.ReadDWord;
  Result.WaveMacroRelease := S.ReadDWord;
  Result.PitchMacroRelease := S.ReadDWord;
  Result.Extra1MacroRelease := S.ReadDWord;
  Result.Extra2MacroRelease := S.ReadDWord;
  Result.Extra3MacroRelease := S.ReadDWord;
  Result.AlgMacroRelease := S.ReadDWord;
  Result.FbMacroRelease := S.ReadDWord;
  Result.FmsMacroRelease := S.ReadDWord;
  Result.AmsMacroRelease := S.ReadDWord;
  for I := 1 to 4 do begin // --- | **operator release points** × 4 (>=44)
    Result.AmMacroRelease := S.ReadDWord;
    Result.ArMacroRelease := S.ReadDWord;
    Result.DrMacroRelease := S.ReadDWord;
    Result.MultMacroRelease := S.ReadDWord;
    Result.RrMacroRelease := S.ReadDWord;
    Result.SlMacroRelease := S.ReadDWord;
    Result.TlMacroRelease := S.ReadDWord;
    Result.Dt2MacroRelease := S.ReadDWord;
    Result.RsMacroRelease := S.ReadDWord;
    Result.DtMacroRelease := S.ReadDWord;
    Result.D2RMacroRelease := S.ReadDWord;
    Result.SsgEgMacroRelease := S.ReadDWord;
  end;
  for I := 1 to 4 do begin // --- | **extended op macro headers** × 4 (>=61)
    Result.DamMacroLength := S.ReadDWord;
    Result.DvbMacroLength := S.ReadDWord;
    Result.EgtMacroLength := S.ReadDWord;
    Result.KslMacroLength := S.ReadDWord;
    Result.SusMacroLength := S.ReadDWord;
    Result.VibMacroLength := S.ReadDWord;
    Result.WsMacroLength := S.ReadDWord;
    Result.KsrMacroLength := S.ReadDWord;
    Result.DamMacroLoop := S.ReadDWord;
    Result.DvbMacroLoop := S.ReadDWord;
    Result.EgtMacroLoop := S.ReadDWord;
    Result.KslMacroLoop := S.ReadDWord;
    Result.SusMacroLoop := S.ReadDWord;
    Result.VibMacroLoop := S.ReadDWord;
    Result.WsMacroLoop := S.ReadDWord;
    Result.KsrMacroLoop := S.ReadDWord;
    Result.DamMacroRelease := S.ReadDWord;
    Result.DvbMacroRelease := S.ReadDWord;
    Result.EgtMacroRelease := S.ReadDWord;
    Result.KslMacroRelease := S.ReadDWord;
    Result.SusMacroRelease := S.ReadDWord;
    Result.VibMacroRelease := S.ReadDWord;
    Result.WsMacroRelease := S.ReadDWord;
    Result.KsrMacroRelease := S.ReadDWord;
    Result.DamMacroOpen := S.ReadByte;
    Result.DvbMacroOpen := S.ReadByte;
    Result.EgtMacroOpen := S.ReadByte;
    Result.KslMacroOpen := S.ReadByte;
    Result.SusMacroOpen := S.ReadByte;
    Result.VibMacroOpen := S.ReadByte;
    Result.WsMacroOpen := S.ReadByte;
    Result.KsrMacroOpen := S.ReadByte;
  end;
  for I := 1 to 4 do begin //  --- | **extended op macros** × 4 (>=61)
    SetLength(Result.DamMacro, Result.DamMacroLength);
    if Length(Result.DamMacro) <> 0 then
      S.ReadBuffer(Result.DamMacro[0], SizeOf(UInt8)*Result.DamMacroLength);
    SetLength(Result.DvbMacro, Result.DvbMacroLength);
    if Length(Result.DvbMacro) <> 0 then
      S.ReadBuffer(Result.DvbMacro[0], SizeOf(UInt8)*Result.DvbMacroLength);
    SetLength(Result.EgtMacro, Result.EgtMacroLength);
    if Length(Result.EgtMacro) <> 0 then
      S.ReadBuffer(Result.EgtMacro[0], SizeOf(UInt8)*Result.EgtMacroLength);
    SetLength(Result.KslMacro, Result.KslMacroLength);
    if Length(Result.KslMacro) <> 0 then
      S.ReadBuffer(Result.KslMacro[0], SizeOf(UInt8)*Result.KslMacroLength);
    SetLength(Result.SusMacro, Result.SusMacroLength);
    if Length(Result.SusMacro) <> 0 then
      S.ReadBuffer(Result.SusMacro[0], SizeOf(UInt8)*Result.SusMacroLength);
    SetLength(Result.VibMacro, Result.VibMacroLength);
    if Length(Result.VibMacro) <> 0 then
      S.ReadBuffer(Result.VibMacro[0], SizeOf(UInt8)*Result.VibMacroLength);
    SetLength(Result.WsMacro, Result.WsMacroLength);
    if Length(Result.WsMacro) <> 0 then
      S.ReadBuffer(Result.WsMacro[0], SizeOf(UInt8)*Result.WsMacroLength);
    SetLength(Result.KsrMacro, Result.KsrMacroLength);
    if Length(Result.KsrMacro) <> 0 then
      S.ReadBuffer(Result.KsrMacro[0], SizeOf(UInt8)*Result.KsrMacroLength);
  end;
  Result.FixedFrequencyMode := S.ReadByte;
  Result.Reserved8 := S.ReadByte;
  Result.KickFrequency := S.ReadWord;
  Result.SnareHiHatFrequency := S.ReadWord;
  Result.TomTopFrequency := S.ReadWord;
  Result.UseNoteMap := S.ReadByte;
  SetLength(Result.NoteFrequency120, 120);
  if Length(Result.NoteFrequency120) <> 0 then
    S.ReadBuffer(Result.NoteFrequency120[0], SizeOf(UInt32)*120);
  SetLength(Result.NoteSample120, 120);
  if Length(Result.NoteSample120) <> 0 then
    S.ReadBuffer(Result.NoteSample120[0], SizeOf(UInt16)*120);
  Result.InitialWaveform := S.ReadDWord;
  Result.WavePosition := S.ReadByte;
  Result.WaveLength := S.ReadByte;
  Result.WaveMode := S.ReadByte;
  Result.Reserved9 := S.ReadByte;
  Result.LeftPanningMacroLength := S.ReadDWord;
  Result.RightPanningMacroLength := S.ReadDWord;
  Result.PhaseResetMacroLength := S.ReadDWord;
  Result.Extra4MacroLength := S.ReadDWord;
  Result.Extra5MacroLength := S.ReadDWord;
  Result.Extra6MacroLength := S.ReadDWord;
  Result.Extra7MacroLength := S.ReadDWord;
  Result.Extra8MacroLength := S.ReadDWord;
  Result.LeftPanningMacroLoop := S.ReadDWord;
  Result.RightPanningMacroLoop := S.ReadDWord;
  Result.PhaseResetMacroLoop := S.ReadDWord;
  Result.Extra4MacroLoop := S.ReadDWord;
  Result.Extra5MacroLoop := S.ReadDWord;
  Result.Extra6MacroLoop := S.ReadDWord;
  Result.Extra7MacroLoop := S.ReadDWord;
  Result.Extra8MacroLoop := S.ReadDWord;
  Result.LeftPanningMacroRelease := S.ReadDWord;
  Result.RightPanningMacroRelease := S.ReadDWord;
  Result.PhaseResetMacroRelease := S.ReadDWord;
  Result.Extra4MacroRelease := S.ReadDWord;
  Result.Extra5MacroRelease := S.ReadDWord;
  Result.Extra6MacroRelease := S.ReadDWord;
  Result.Extra7MacroRelease := S.ReadDWord;
  Result.Extra8MacroRelease := S.ReadDWord;
  Result.LeftPanningMacroOpen := S.ReadByte;
  Result.RightPanningMacroOpen := S.ReadByte;
  Result.PhaseResetMacroOpen := S.ReadByte;
  Result.Extra4MacroOpen := S.ReadByte;
  Result.Extra5MacroOpen := S.ReadByte;
  Result.Extra6MacroOpen := S.ReadByte;
  Result.Extra7MacroOpen := S.ReadByte;
  Result.Extra8MacroOpen := S.ReadByte;
  SetLength(Result.LeftPanningMacro, Result.LeftPanningMacroLength);
  if Length(Result.LeftPanningMacro) <> 0 then
    S.ReadBuffer(Result.LeftPanningMacro[0], SizeOf(UInt32)*Result.LeftPanningMacroLength);
  SetLength(Result.RightPanningMacro, Result.RightPanningMacroLength);
  if Length(Result.RightPanningMacro) <> 0 then
    S.ReadBuffer(Result.RightPanningMacro[0], SizeOf(UInt32)*Result.RightPanningMacroLength);
  SetLength(Result.PhaseResetMacro, Result.PhaseResetMacroLength);
  if Length(Result.PhaseResetMacro) <> 0 then
    S.ReadBuffer(Result.PhaseResetMacro[0], SizeOf(UInt32)*Result.PhaseResetMacroLength);
  SetLength(Result.Extra4Macro, Result.Extra4MacroLength);
  if Length(Result.Extra4Macro) <> 0 then
    S.ReadBuffer(Result.Extra4Macro[0], SizeOf(UInt32)*Result.Extra4MacroLength);
  SetLength(Result.Extra5Macro, Result.Extra5MacroLength);
  if Length(Result.Extra5Macro) <> 0 then
    S.ReadBuffer(Result.Extra5Macro[0], SizeOf(UInt32)*Result.Extra5MacroLength);
  SetLength(Result.Extra6Macro, Result.Extra6MacroLength);
  if Length(Result.Extra6Macro) <> 0 then
    S.ReadBuffer(Result.Extra6Macro[0], SizeOf(UInt32)*Result.Extra6MacroLength);
  SetLength(Result.Extra7Macro, Result.Extra7MacroLength);
  if Length(Result.Extra7Macro) <> 0 then
    S.ReadBuffer(Result.Extra7Macro[0], SizeOf(UInt32)*Result.Extra7MacroLength);
  SetLength(Result.Extra8Macro, Result.Extra8MacroLength);
  if Length(Result.Extra8Macro) <> 0 then
    S.ReadBuffer(Result.Extra8Macro[0], SizeOf(UInt32)*Result.Extra8MacroLength);
  Result.ModulationSpeed := S.ReadDWord;
  Result.ModulationDepth := S.ReadDWord;
  Result.InitModulationTableWithFirstWave := S.ReadByte;
  S.ReadBuffer(Result.Reserved10[0], SizeOf(Byte)*3);
  S.ReadBuffer(Result.ModulationTable[0], SizeOf(Byte)*32);
  Result.Fms2 := S.ReadByte;
  Result.Ams2 := S.ReadByte;
  Result.FirstWave := S.ReadDWord;
  Result.SecondWave := S.ReadDWord;
  Result.RateDivider := S.ReadByte;
  Result.Effect := S.ReadByte;
  Result.Enabled := S.ReadByte;
  Result.Global := S.ReadByte;
  Result.Speed1 := S.ReadByte;
  Result.Parameter1 := S.ReadByte;
  Result.Parameter2 := S.ReadByte;
  Result.Parameter3 := S.ReadByte;
  Result.Parameter4 := S.ReadByte;
  Result.VolumeMacroMode := S.ReadByte;
  Result.DutyMacroMode := S.ReadByte;
  Result.WaveMacroMode := S.ReadByte;
  Result.PitchMacroMode := S.ReadByte;
  Result.Extra1MacroMode := S.ReadByte;
  Result.Extra2MacroMode := S.ReadByte;
  Result.Extra3MacroMode := S.ReadByte;
  Result.AlgMacroMode := S.ReadByte;
  Result.FbMacroMode := S.ReadByte;
  Result.FmsMacroMode := S.ReadByte;
  Result.AmsMacroMode := S.ReadByte;
  Result.LeftPanningMacroMode := S.ReadByte;
  Result.RightPanningMacroMode := S.ReadByte;
  Result.PhaseResetMacroMode := S.ReadByte;
  Result.Extra4MacroMode := S.ReadByte;
  Result.Extra5MacroMode := S.ReadByte;
  Result.Extra6MacroMode := S.ReadByte;
  Result.Extra7MacroMode := S.ReadByte;
  Result.Extra8MacroMode := S.ReadByte;
  Result.DonTTestGateBeforeNewNote := S.ReadByte;
  Result.AttackRate := S.ReadByte;
  Result.Decay1Rate := S.ReadByte;
  Result.DecayLevel := S.ReadByte;
  Result.Decay2Rate := S.ReadByte;
  Result.ReleaseRate := S.ReadByte;
  Result.RateCorrection := S.ReadByte;
  Result.LfoRate := S.ReadByte;
  Result.VibDepth := S.ReadByte;
  Result.AmDepth := S.ReadByte;
  S.ReadBuffer(Result.Reserved11[0], SizeOf(Byte)*23);
  Result.UseSample := S.ReadByte;
  Result.SwitchRolesOfPhaseResetTimerAndFrequency := S.ReadByte;
  Result.Length2 := S.ReadByte;
  SetLength(Result.HardwareSequenceData, Result.Length2*3);
  if Length(Result.HardwareSequenceData) <> 0 then
    S.ReadBuffer(Result.HardwareSequenceData[0], SizeOf(UInt8)*Length(Result.HardwareSequenceData));
  Result.UseSoftwareEnvelope := S.ReadByte;
  Result.AlwaysInitHardEnvOnNewNote := S.ReadByte;
  Result.FilterMode := S.ReadByte;
  Result.K1 := S.ReadWord;
  Result.K2 := S.ReadWord;
  Result.EnvelopeCount := S.ReadWord;
  Result.LeftVolumeRamp := S.ReadByte;
  Result.RightVolumeRamp := S.ReadByte;
  Result.K1Ramp := S.ReadByte;
  Result.K2Ramp := S.ReadByte;
  Result.K1Slow := S.ReadByte;
  Result.K2Slow := S.ReadByte;
  Result.UseEnvelope := S.ReadByte;
  Result.GainMode := S.ReadByte;
  Result.Gain := S.ReadByte;
  Result.Attack2 := S.ReadByte;
  Result.Decay2 := S.ReadByte;
  Result.Sustain2 := S.ReadByte;
  Result.Release2 := S.ReadByte;
  Result.VolumeMacroSpeed := S.ReadByte;
  Result.ArpMacroSpeed := S.ReadByte;
  Result.DutyMacroSpeed := S.ReadByte;
  Result.WaveMacroSpeed := S.ReadByte;
  Result.PitchMacroSpeed := S.ReadByte;
  Result.Extra1MacroSpeed := S.ReadByte;
  Result.Extra2MacroSpeed := S.ReadByte;
  Result.Extra3MacroSpeed := S.ReadByte;
  Result.AlgMacroSpeed := S.ReadByte;
  Result.FbMacroSpeed := S.ReadByte;
  Result.FmsMacroSpeed := S.ReadByte;
  Result.AmsMacroSpeed := S.ReadByte;
  Result.LeftPanningMacroSpeed := S.ReadByte;
  Result.RightPanningMacroSpeed := S.ReadByte;
  Result.PhaseResetMacroSpeed := S.ReadByte;
  Result.Extra4MacroSpeed := S.ReadByte;
  Result.Extra5MacroSpeed := S.ReadByte;
  Result.Extra6MacroSpeed := S.ReadByte;
  Result.Extra7MacroSpeed := S.ReadByte;
  Result.Extra8MacroSpeed := S.ReadByte;
  Result.VolumeMacroDelay := S.ReadByte;
  Result.ArpMacroDelay := S.ReadByte;
  Result.DutyMacroDelay := S.ReadByte;
  Result.WaveMacroDelay := S.ReadByte;
  Result.PitchMacroDelay := S.ReadByte;
  Result.Extra1MacroDelay := S.ReadByte;
  Result.Extra2MacroDelay := S.ReadByte;
  Result.Extra3MacroDelay := S.ReadByte;
  Result.AlgMacroDelay := S.ReadByte;
  Result.FbMacroDelay := S.ReadByte;
  Result.FmsMacroDelay := S.ReadByte;
  Result.AmsMacroDelay := S.ReadByte;
  Result.LeftPanningMacroDelay := S.ReadByte;
  Result.RightPanningMacroDelay := S.ReadByte;
  Result.PhaseResetMacroDelay := S.ReadByte;
  Result.Extra4MacroDelay := S.ReadByte;
  Result.Extra5MacroDelay := S.ReadByte;
  Result.Extra6MacroDelay := S.ReadByte;
  Result.Extra7MacroDelay := S.ReadByte;
  Result.Extra8MacroDelay := S.ReadByte;
  for I := 1 to 4 do begin // --- | **operator macro speeds/delay** × 4 (>=111)
    Result.AmMacroSpeed := S.ReadByte;
    Result.ArMacroSpeed := S.ReadByte;
    Result.DrMacroSpeed := S.ReadByte;
    Result.MultMacroSpeed := S.ReadByte;
    Result.RrMacroSpeed := S.ReadByte;
    Result.SlMacroSpeed := S.ReadByte;
    Result.TlMacroSpeed := S.ReadByte;
    Result.Dt2MacroSpeed := S.ReadByte;
    Result.RsMacroSpeed := S.ReadByte;
    Result.DtMacroSpeed := S.ReadByte;
    Result.D2RMacroSpeed := S.ReadByte;
    Result.SsgEgMacroSpeed := S.ReadByte;
    Result.DamMacroSpeed := S.ReadByte;
    Result.DvbMacroSpeed := S.ReadByte;
    Result.EgtMacroSpeed := S.ReadByte;
    Result.KslMacroSpeed := S.ReadByte;
    Result.SusMacroSpeed := S.ReadByte;
    Result.VibMacroSpeed := S.ReadByte;
    Result.WsMacroSpeed := S.ReadByte;
    Result.KsrMacroSpeed := S.ReadByte;
    Result.AmMacroDelay := S.ReadByte;
    Result.ArMacroDelay := S.ReadByte;
    Result.DrMacroDelay := S.ReadByte;
    Result.MultMacroDelay := S.ReadByte;
    Result.RrMacroDelay := S.ReadByte;
    Result.SlMacroDelay := S.ReadByte;
    Result.TlMacroDelay := S.ReadByte;
    Result.Dt2MacroDelay := S.ReadByte;
    Result.RsMacroDelay := S.ReadByte;
    Result.DtMacroDelay := S.ReadByte;
    Result.D2RMacroDelay := S.ReadByte;
    Result.SsgEgMacroDelay := S.ReadByte;
    Result.DamMacroDelay := S.ReadByte;
    Result.DvbMacroDelay := S.ReadByte;
    Result.EgtMacroDelay := S.ReadByte;
    Result.KslMacroDelay := S.ReadByte;
    Result.SusMacroDelay := S.ReadByte;
    Result.VibMacroDelay := S.ReadByte;
    Result.WsMacroDelay := S.ReadByte;
    Result.KsrMacroDelay := S.ReadByte;
  end;
end;

function ReadFURSongInfo(S: TFURStream): TFURSongInfo;
var
  I: Integer;
begin
  Result.InfoBlockId := S.ReadDWord;
  Result.SizeOfThisBlock := S.ReadDWord;
  Result.TimeBaseOfFirstSong := S.ReadByte;
  Result.Speed1OfFirstSong := S.ReadByte;
  Result.Speed2OfFirstSong := S.ReadByte;
  Result.InitialArpeggioTimeOfFirstSong := S.ReadByte;
  Result.TicksPerSecondOfFirstSong := S.ReadSingle;
  Result.PatternLengthOfFirstSong := S.ReadWord;
  Result.OrdersLengthOfFirstSong := S.ReadWord;
  Result.HighlightAOfFirstSong := S.ReadByte;
  Result.HighlightBOfFirstSong := S.ReadByte;
  Result.InstrumentCount := S.ReadWord;
  Result.WavetableCount := S.ReadWord;
  Result.SampleCount := S.ReadWord;
  Result.PatternCountGlobal := S.ReadDWord;
  S.ReadBuffer(Result.ListOfSoundChips[0], SizeOf(Byte)*32);
  S.ReadBuffer(Result.SoundChipVolumes[0], SizeOf(Byte)*32);
  S.ReadBuffer(Result.SoundChipPanning[0], SizeOf(Byte)*32);
  S.ReadBuffer(Result.SoundChipParameters[0], SizeOf(Byte)*128);
  Result.SongName := S.ReadFURString;
  Result.SongAuthor := S.ReadFURString;
  Result.A4Tuning := S.ReadSingle;
  Result.LimitSlides := S.ReadByte;
  Result.LinearPitch := S.ReadByte;
  Result.LoopModality := S.ReadByte;
  Result.ProperNoiseLayout := S.ReadByte;
  Result.WaveDutyIsVolume := S.ReadByte;
  Result.ResetMacroOnPorta := S.ReadByte;
  Result.LegacyVolumeSlides := S.ReadByte;
  Result.CompatibleArpeggio := S.ReadByte;
  Result.NoteOffResetsSlides := S.ReadByte;
  Result.TargetResetsSlides := S.ReadByte;
  Result.ArpeggioInhibitsPortamento := S.ReadByte;
  Result.WackAlgorithmMacro := S.ReadByte;
  Result.BrokenShortcutSlides := S.ReadByte;
  Result.IgnoreDuplicateSlides := S.ReadByte;
  Result.StopPortamentoOnNoteOff := S.ReadByte;
  Result.ContinuousVibrato := S.ReadByte;
  Result.BrokenDacMode := S.ReadByte;
  Result.OneTickCut := S.ReadByte;
  Result.InstrumentChangeAllowedDuringPorta := S.ReadByte;
  Result.ResetNoteBaseOnArpeggioEffectStop0000 := S.ReadByte;
  SetLength(Result.PointersToInstruments, Result.InstrumentCount);
  if Length(Result.PointersToInstruments) <> 0 then
    S.ReadBuffer(Result.PointersToInstruments[0], SizeOf(UInt32)*Result.InstrumentCount);
  SetLength(Result.PointersToWavetables, Result.WavetableCount);
  if Length(Result.PointersToWavetables) <> 0 then
    S.ReadBuffer(Result.PointersToWavetables[0], SizeOf(UInt32)*Result.WavetableCount);
  SetLength(Result.PointersToSamples, Result.SampleCount);
  if Length(Result.PointersToSamples) <> 0 then
    S.ReadBuffer(Result.PointersToSamples[0], SizeOf(UInt32)*Result.SampleCount);
  SetLength(Result.PointersToPatterns, Result.PatternCountGlobal);
  if Length(Result.PointersToPatterns) <> 0 then
    S.ReadBuffer(Result.PointersToPatterns[0], SizeOf(UInt32)*Result.PatternCountGlobal);


  SetLength(Result.OrdersOfFirstSong, 4);
  for I := Low(Result.OrdersOfFirstSong) to High(Result.OrdersOfFirstSong) do begin
    SetLength(Result.OrdersOfFirstSong[I], Result.OrdersLengthOfFirstSong);
    S.ReadBuffer(Result.OrdersOfFirstSong[I, 0], SizeOf(UInt8)*Result.OrdersLengthOfFirstSong);
  end;

  SetLength(Result.EffectColumnsOfFirstSong, 4); // TODO
  if Length(Result.EffectColumnsOfFirstSong) <> 0 then
    S.ReadBuffer(Result.EffectColumnsOfFirstSong[0], SizeOf(UInt8)*4);
  SetLength(Result.ChannelHideStatusOfFirstSong, 4);
  if Length(Result.ChannelHideStatusOfFirstSong) <> 0 then
    S.ReadBuffer(Result.ChannelHideStatusOfFirstSong[0], SizeOf(UInt8)*4);
  SetLength(Result.ChannelCollapseStatusOfFirstSong, 4);
  if Length(Result.ChannelCollapseStatusOfFirstSong) <> 0 then
    S.ReadBuffer(Result.ChannelCollapseStatusOfFirstSong[0], SizeOf(UInt8)*4);
  SetLength(Result.ChannelNamesOfFirstSong, 4);
  for I := Low(Result.ChannelNamesOfFirstSong) to High(Result.ChannelNamesOfFirstSong) do
    Result.ChannelNamesOfFirstSong[I] := S.ReadFURString;
  SetLength(Result.ChannelShortNamesOfFirstSong, 4);
  for I := Low(Result.ChannelShortNamesOfFirstSong) to High(Result.ChannelShortNamesOfFirstSong) do
    Result.ChannelShortNamesOfFirstSong[I] := S.ReadFURString;
  Result.SongComment := S.ReadFURString;
  Result.MasterVolume10F100 := S.ReadSingle;
  Result.BrokenSpeedSelection := S.ReadByte;
  Result.NoSlidesOnFirstTick := S.ReadByte;
  Result.NextRowResetArpPos := S.ReadByte;
  Result.IgnoreJumpAtEnd := S.ReadByte;
  Result.BuggyPortamentoAfterSlide := S.ReadByte;
  Result.NewInsAffectsEnvelopeGameBoy := S.ReadByte;
  Result.ExtchChannelStateIsShared := S.ReadByte;
  Result.IgnoreDacModeChangeOutsideOfIntendedChannel := S.ReadByte;
  Result.E1XyAndE2XyAlsoTakePriorityOverSlide00 := S.ReadByte;
  Result.NewSegaPcmWithMacrosAndProperVolPan := S.ReadByte;
  Result.WeirdFNumBlockBasedChipPitchSlides := S.ReadByte;
  Result.SnDutyMacroAlwaysResetsPhase := S.ReadByte;
  Result.PitchMacroIsLinear := S.ReadByte;
  Result.PitchSlideSpeedInFullLinearPitchMode := S.ReadByte;
  Result.OldOctaveBoundaryBehavior := S.ReadByte;
  Result.DisableOpn2DacVolumeControl := S.ReadByte;
  Result.NewVolumeScalingStrategy := S.ReadByte;
  Result.VolumeMacroStillAppliesAfterEnd := S.ReadByte;
  Result.BrokenOutvol := S.ReadByte;
  Result.E1XyAndE2XyStopOnSameNote := S.ReadByte;
  Result.BrokenInitialPositionOfPortaAfterArp := S.ReadByte;
  Result.SnPeriodsUnder8AreTreatedAs1 := S.ReadByte;
  S.ReadBuffer(Result.Reserved[0], SizeOf(Byte)*6);
  Result.VirtualTempoNumeratorOfFirstSong := S.ReadWord;
  Result.VirtualTempoDenominatorOfFirstSong := S.ReadWord;
  Result.FirstSubsongName := S.ReadFURString;
  Result.FirstSubsongComment := S.ReadFURString;
  Result.NumberOfAdditionalSubsongs := S.ReadByte;
  S.ReadBuffer(Result.Reserved[0], SizeOf(Byte)*3);
  SetLength(Result.PointersToSubsongData, Result.NumberOfAdditionalSubsongs);
  if Length(Result.PointersToSubsongData) <> 0 then
    S.ReadBuffer(Result.PointersToSubsongData[0], SizeOf(UInt32)*Result.NumberOfAdditionalSubsongs);
  Result.SystemName := S.ReadFURString;
  Result.AlbumCategoryGameName := S.ReadFURString;
  Result.SongNameJapanese := S.ReadFURString;
  Result.SongAuthorJapanese := S.ReadFURString;
  Result.SystemNameJapanese := S.ReadFURString;
  Result.AlbumCategoryGameNameJapanese := S.ReadFURString;
end;

procedure TranslateEffect(EffectCode, EffectParams: Byte; out OCode: Integer; out OParams: Byte);
begin
  OParams := EffectParams;

  case EffectCode of
    $0, $1, $2, $3, $4, $A, $B, $D, $F: begin
      OCode := EffectCode;
      OParams := EffectParams;
    end;

    $ED: begin
      OCode := $7;
      OParams := EffectParams;
    end

    else begin
      OCode := $0;
      OParams := $00;
    end;
  end;
end;

procedure TranslatePattern(const Pat: TFURPattern; PPat: PPattern);
var
  I, J: Integer;
  FRow: TFURRow;
begin
  for I := Low(Pat.PatternData) to High(Pat.PatternData) do begin
    FRow := Pat.PatternData[I];
    with PPat^[I] do begin
      if FRow.Note = 100 then begin // Note cut
        EffectCode := $E;
        EffectParams.Value := $00;
      end else if FRow.Note <> 0 then
        Note := FRow.Note + ((FRow.Octave-2) * 12);

      if FRow.Instrument <> -1 then
        Instrument := FRow.Instrument + 1;

      for J := Low(FRow.Effects) to High(FRow.Effects) do
        if (FRow.Effects[J].EffectCode <> -1) and (FRow.Effects[J].EffectParams <> -1) then begin
          TranslateEffect(
            FRow.Effects[J].EffectCode,
            FRow.Effects[J].EffectParams,
            EffectCode,
            EffectParams.Value
          );
          {if EffectCode in ContinuousEffects then
            Volume := 1; // Mark as continuous}
        end;

      if (EffectCode = 0) and (EffectParams.Value = 0) and (FRow.Volume <> -1) then begin
        EffectCode := $C;
        EffectParams.Value := FRow.Volume;
      end;
    end;
  end;
end;

function LoadSongFromFURStream(Stream: TStream): TSong;
var
  S: TFURStream;
  Header: TFURHeader;
  SongInfo: TFURSongInfo;
  Pat: TFURPattern;
  Instr: TFURInstrument;

  PPatHT: PPattern;
  I, J: Integer;
begin
  InitializeSong(Result);
  S := TFURStream.create(Stream);

  Header := ReadFURHeader(S);

  S.Seek(Header.SongInfoPointer, soBeginning);
  SongInfo := ReadFURSongInfo(S);

  // Copy order table over
  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do begin
    SetLength(Result.OrderMatrix[I], SongInfo.OrdersLengthOfFirstSong+1); // off-by-one error on my part
    for J := 0 to SongInfo.OrdersLengthOfFirstSong-1 do
      Result.OrderMatrix[I, J] := ((I+1)*100) + SongInfo.OrdersOfFirstSong[I, J];
  end;

  // Translate patterns
  for I in SongInfo.PointersToPatterns do begin
    S.Seek(I, soBeginning);
    Pat := ReadFURPattern(S, SongInfo.PatternLengthOfFirstSong, SongInfo.EffectColumnsOfFirstSong);
    PPatHT := Result.Patterns.GetOrCreateNew(((Pat.Channel+1)*100)+Pat.PatternIndex);
    TranslatePattern(Pat, PPatHT);
    CleanupPattern(PPatHT);
  end;

  // Load instruments
  for I in SongInfo.PointersToInstruments do begin
    S.Seek(I, soBeginning);
    Instr := ReadFURInstrument(S);
    writeln;
  end;
end;

{ TFURStream }

function TFURStream.ReadSingle: Single;
begin
  ReadBuffer(Result, SizeOf(Single));
end;

function TFURStream.ReadFURString: String;
var
  B: Byte;
begin
  Result := '';
  B := ReadByte;
  while B <> 0 do begin
    Result += Chr(B);
    B := ReadByte;
  end;
end;

end.

