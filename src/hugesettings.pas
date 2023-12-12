unit hUGESettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles, Constants, Forms, Dialogs;

type

  { TTrackerSettings }

  TTrackerSettings = class
  private
    FDisplayRowNumbersAsHex: Boolean;
    FDisplayOrderRowNumbersAsHex: Boolean;
    FPreviewWhenBumping: Boolean;
    FPreviewWhenPlacing: Boolean;
    FDrawWaveformGrid: Boolean;
    FVerticalTabs: Boolean;
    SettingsFile: TINIFile;

    FPatternEditorFontSize: Integer;
    FUseScopes, FUseCustomKeymap: Boolean;

    procedure SetDisplayOrderRowNumbersAsHex(AValue: Boolean);
    procedure SetDisplayRowNumbersAsHex(AValue: Boolean);
    procedure SetDrawWaveformGrid(AValue: Boolean);
    procedure SetVerticalTabs(AValue: Boolean);
    procedure SetPatternEditorFontSize(AValue: Integer);
    procedure SetPreviewWhenBumping(AValue: Boolean);
    procedure SetPreviewWhenPlacing(AValue: Boolean);
    procedure SetUseCustomKeymap(AValue: Boolean);
    procedure SetUseScopes(AValue: Boolean);
  public
    property PatternEditorFontSize: Integer read FPatternEditorFontSize write SetPatternEditorFontSize;
    property UseScopes: Boolean read FUseScopes write SetUseScopes;
    property UseCustomKeymap: Boolean read FUseCustomKeymap write SetUseCustomKeymap;
    property PreviewWhenPlacing: Boolean read FPreviewWhenPlacing write SetPreviewWhenPlacing;
    property PreviewWhenBumping: Boolean read FPreviewWhenBumping write SetPreviewWhenBumping;
    property DisplayRowNumbersAsHex: Boolean read FDisplayRowNumbersAsHex write SetDisplayRowNumbersAsHex;
    property DisplayOrderRowNumbersAsHex: Boolean read FDisplayOrderRowNumbersAsHex write SetDisplayOrderRowNumbersAsHex;
    property DrawWaveformGrid: Boolean read FDrawWaveformGrid write SetDrawWaveformGrid;
    property VerticalTabs: Boolean read FVerticalTabs write SetVerticalTabs;

    constructor Create;
  end;

procedure InitializeTrackerSettings;

var
  TrackerSettings: TTrackerSettings = nil;

implementation

procedure SetupDirectoryParameter(Param: String; Default: String; out Variable: ShortString);
var
  S: String;
begin
  Variable := Default;

  S := Application.GetOptionValue(Param);
  if S <> '' then begin
    if not DirectoryExists(S) then
      ShowMessage('Specified '+Param+' does not exist. Using '+Default+' instead.')
    else
      Variable := S;
  end;
end;

procedure InitializeTrackerSettings;
var
  XDGConfigDir, XDGCacheDir, MacConfigDir, MacCacheDir: String;
begin
  {$ifdef MSWINDOWS}
  SetupDirectoryParameter('conf_dir', '.', ConfDir);
  SetupDirectoryParameter('cache_dir', '.', CacheDir);
  SetupDirectoryParameter('runtime_dir', '.', RuntimeDir);
  {$endif}

  {$if defined(LINUX) or defined(FREEBSD) or defined(OPENBSD)}
  XDGConfigDir := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if XDGConfigDir = '' then
    XDGConfigDir := ConcatPaths([GetEnvironmentVariable('HOME'), '.config', 'hUGETracker']);

  XDGCacheDir := GetEnvironmentVariable('XDG_CACHE_HOME');
  if XDGCacheDir = '' then
    XDGCacheDir := ConcatPaths([GetEnvironmentVariable('HOME'), '.cache', 'hUGETracker']);

  if not DirectoryExists(XDGConfigDir) then CreateDir(XDGConfigDir);
  if not DirectoryExists(XDGCacheDir) then CreateDir(XDGCacheDir);

  SetupDirectoryParameter('conf_dir', XDGConfigDir, ConfDir);
  SetupDirectoryParameter('cache_dir', XDGCacheDir, CacheDir);
  SetupDirectoryParameter('runtime_dir', '.', RuntimeDir);
  {$endif}

  {$ifdef DARWIN}
  MacConfigDir := ConcatPaths([GetEnvironmentVariable('HOME'), '.config', 'hUGETracker']);
  MacCacheDir := ConcatPaths([GetEnvironmentVariable('HOME'), '.cache', 'hUGETracker']);

  if not DirectoryExists(MacConfigDir) then CreateDir(MacConfigDir);
  if not DirectoryExists(MacCacheDir) then CreateDir(MacCacheDir);

  SetupDirectoryParameter('conf_dir', MacConfigDir, ConfDir);
  SetupDirectoryParameter('cache_dir', MacCacheDir, CacheDir);
  SetupDirectoryParameter('runtime_dir', '../Resources', RuntimeDir);
  {$endif}

  TrackerSettings := TTrackerSettings.Create;
end;

{ TTrackerSettings }

procedure TTrackerSettings.SetPatternEditorFontSize(AValue: Integer);
begin
  FPatternEditorFontSize := AValue;
  SettingsFile.WriteInteger('hUGETracker', 'fontsize', AValue);
end;

procedure TTrackerSettings.SetDisplayRowNumbersAsHex(AValue: Boolean);
begin
  FDisplayRowNumbersAsHex:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'DisplayRowNumbersAsHex', AValue);
end;

procedure TTrackerSettings.SetDrawWaveformGrid(AValue: Boolean);
begin
  FDrawWaveformGrid:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'DrawWaveformGrid', AValue);
end;

procedure TTrackerSettings.SetVerticalTabs(AValue: Boolean);
begin
  FVerticalTabs:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'VerticalTabs', AValue);
end;

procedure TTrackerSettings.SetDisplayOrderRowNumbersAsHex(AValue: Boolean);
begin
  FDisplayOrderRowNumbersAsHex:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'DisplayOrderRowNumbersAsHex', AValue);
end;

procedure TTrackerSettings.SetPreviewWhenBumping(AValue: Boolean);
begin
  FPreviewWhenBumping:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'PreviewWhenBumping', AValue);
end;

procedure TTrackerSettings.SetPreviewWhenPlacing(AValue: Boolean);
begin
  FPreviewWhenPlacing:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'PreviewWhenPlacing', AValue);
end;

procedure TTrackerSettings.SetUseCustomKeymap(AValue: Boolean);
begin
  FUseCustomKeymap := AValue;
  SettingsFile.WriteBool('hUGETracker', 'CustomKeymap', AValue);
end;

procedure TTrackerSettings.SetUseScopes(AValue: Boolean);
begin
  FUseScopes := AValue;
  SettingsFile.WriteBool('hUGETracker', 'ScopesOn', AValue);
end;

constructor TTrackerSettings.Create;
begin
  SettingsFile := TINIFile.Create(ConcatPaths([ConfDir, 'options.ini']));

  FPatternEditorFontSize := SettingsFile.ReadInteger('hUGETracker', 'fontsize', 12);
  FUseScopes := SettingsFile.ReadBool('hUGETracker', 'ScopesOn', False);
  FUseCustomKeymap := SettingsFile.ReadBool('hUGETracker', 'CustomKeymap', False);
  FPreviewWhenPlacing := SettingsFile.ReadBool('hUGETracker', 'PreviewWhenPlacing', True);
  FPreviewWhenBumping := SettingsFile.ReadBool('hUGETracker', 'PreviewWhenBumping', False);
  FDisplayRowNumbersAsHex := SettingsFile.ReadBool('hUGETracker', 'DisplayRowNumbersAsHex', False);
  FDisplayOrderRowNumbersAsHex := SettingsFile.ReadBool('hUGETracker', 'DisplayOrderRowNumbersAsHex', False);
  FDrawWaveformGrid := SettingsFile.ReadBool('hUGETracker', 'DrawWaveformGrid', False);
  FVerticalTabs := SettingsFile.ReadBool('hUGETracker', 'VerticalTabs', False);
end;

finalization
  if Assigned(TrackerSettings) then TrackerSettings.Free;

end.

