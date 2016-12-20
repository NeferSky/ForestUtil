unit NsUtils;

interface

uses
  Windows, WinSock;

const
  I_SIO_GET_INTERFACE_LIST = $4004747F;
  I_IFF_UP = $00000001;
  I_IFF_BROADCAST = $00000002;
  I_IFF_LOOPBACK = $00000004;
  I_IFF_POINTTOPOINT = $00000008;
  I_IFF_MULTICAST = $00000010;

function WSAIOCtl(S: TSocket;
  Cmd: Dword;
  lpInBuffer: PChar;
  dwInBufferLen: Dword;
  lpOutBuffer: PChar;
  dwOutBufferLen: Dword;
  lpdwOutBytesReturned: lpDword;
  lpOverLapped: Pointer;
  lpOverLappedRoutine: Pointer): Integer; stdcall; external 'WS2_32.DLL';

type
  SockAddrGen = packed record
    AddressIn: SockAddr_In;
    Filler: packed array[0..7] of Char;
  end;

type
  TInterfaceInfo = packed record
    iiFlags: u_long;
    iiAddress: SockAddrGen;
    iiBroadcastAddress: SockAddrGen;
    iiNetmask: SockAddrGen;
  end;

  TInterface = packed record
    IPAddress: string;
    Mask: string;
    Broadcast: string;
    IsUp: Boolean;
    IsBroadcastSupported: Boolean;
    IsLoopback: Boolean;
  end;

  TInterfaces = array of TInterface;

type
  TUserFormat = (ufNameUnknown, ufNameFullyQualifiedDN, ufNameSamCompatible,
    ufNameDisplay, ufNA4, ufNA5, ufNameUniqueId, ufNameCanonical,
    ufNameUserPrincipal, ufNameCanonicalEx, ufNameServicePrincipal, ufNA11,
    ufNameDnsDomain);

type
  TOSVersion = record
    MajorVersion: Dword;
    MinorVersion: Dword;
    BuildNumber: Dword;
    CSDVersion: string;
    PlatformName: string;
  end;

type
  TProcArray = array of string;

type
  TWaitAppType = (watNone, watInput, watClose);

type
  TCharArray = array[0..255] of Char;
  TCharDynArray = array of Char;

  // CMD utils
function HasCmdParam(const ParamName: string; SkipParamCount: Integer = 0):
  Boolean;
procedure RunApp(const CmdLine: string; const DefaultDir: string = '';
  WaitType: TWaitAppType = watNone);
procedure RunElevateApp(const CmdLine: string; const DefaultDir: string = '');
procedure gsShellExecute(const hWindow: HWND; const Operation, FileName: string;
  const Parameters: string = ''; const Directory: string = '';
  const ShowCmd: Integer = SW_SHOWNORMAL);
function GetAppPath: AnsiString;

// SysInfo utils
function GetCPUInfo: string;
function GetCPULevel: string;
function GetCPUSpeed: Double;
function GetPhysicalMemory: Int64;
function GetDriveType_Ns(Drive: Char): string;
function GetDiskFreeSpaceInfo(Drive: Char; out TotalSize, FreeSize: Int64):
  Boolean;
function GetVideoCard: string;
function GetLANIPAddress: string;
//function GetInterfaces(var sInt: String): Boolean;
function GetInterfaces(var Interfaces: TInterfaces): Boolean;
function GetKeyboardLanguage: string;
function GetComputerName_Ns: string;
function GetOSRegInfo(var RegOwner: string; var RegOrg: string): Integer;
function GetWindowsUserName: string;
function GetLoggedOnUserName(UserFormat: TUserFormat): string;
function GetPlatformName: string;
function GetWindowsVersion: string;
function GetOSVersionInfo(var OSVersion: TOSVersion): string;
function GetProcessesWin95(var Processes: TProcArray): Integer;
function GetProcessesWinNT(var Processes: TProcArray): Integer;
function GetWindowsDir: string;
function GetSystemDir: string;
function GetCurrentDir: string;

// File utils
function GetFileInfo(const AFileName: string; out FileSize: Integer;
  out FileTime: Int64): Boolean;
function GetFileVersion(const FileName: string;
  out MajorVersion, MinorVersion: Integer): Boolean;
function GetFullFileVersion(const FileName: string): string;
function GetFullPathOfFile(const FileName: string): string;

// CRC
function CRCMakerPas(InputByte, CRCByte: Byte): Byte;
function CRCMakerAsm(InputByte, CRCByte: Byte): Byte;

// Convert
function StringToArray(Str: string): TCharArray;
function StringToDynArray(Str: string): TCharDynArray;
function DefCurrency(Value: Variant): Currency;
function DefInteger(Value: Variant): Integer;
function BoolToInt(Value: Boolean): Integer;
function StrToBoolean(S: String): Boolean;
function ExtendLeft(Str: String; NewLen: Integer; AddStr: String): String;

const
  VK_O = 79;
  VK_LEFT = 37;
  VK_UP = 38;
  VK_RIGHT = 39;
  VK_DOWN = 40;
  VK_0 = 48;
  VK_1 = 49;
  VK_2 = 50;
  VK_3 = 51;
  VK_4 = 52;
  VK_5 = 53;
  VK_6 = 54;
  VK_7 = 55;
  VK_8 = 56;
  VK_9 = 57;

implementation

uses
  SysUtils, Registry, ActiveX, ShellAPI, TlHelp32, PSAPI, Forms;

//---------------------------------------------------------------------------
{ CMD Utils}

function HasCmdParam(const ParamName: string; SkipParamCount: Integer = 0):
  Boolean;
var
  I: Integer;

begin
  Result := False;

  for I := 1 + SkipParamCount to ParamCount do
    if SameText(ParamStr(I), ParamName) then
    begin
      Result := True;
      Break;
    end;
end;

//---------------------------------------------------------------------------

procedure RunApp(const CmdLine: string; const DefaultDir: string = '';
  WaitType: TWaitAppType = watNone);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;

begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOW;
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil,
    Pointer(DefaultDir), StartupInfo, ProcessInfo) then
    RaiseLastOSError;
  case WaitType of
    watInput: WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    watClose: WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
  end;
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

//---------------------------------------------------------------------------

procedure RunElevateApp(const CmdLine: string; const DefaultDir: string = '');
var
  ShellExecuteInfo: TShellExecuteInfo;

begin
  ShellExecuteInfo.cbSize := SizeOf(TShellExecuteInfo);
  ShellExecuteInfo.fMask := 0;
  ShellExecuteInfo.Wnd := 0;
  ShellExecuteInfo.lpVerb := 'runas';
  ShellExecuteInfo.lpFile := PAnsiChar(CmdLine);
  ShellExecuteInfo.lpParameters := nil;
  ShellExecuteInfo.lpDirectory := PAnsiChar(DefaultDir);
  ShellExecuteInfo.nShow := SW_SHOWNORMAL;
  ShellExecuteEx(@ShellExecuteInfo);
  WaitForSingleObject(ShellExecuteInfo.hProcess, INFINITE);
end;

//---------------------------------------------------------------------------

procedure gsShellExecute(const hWindow: HWND; const Operation, FileName: string;
  const Parameters: string = ''; const Directory: string = '';
  const ShowCmd: Integer = SW_SHOWNORMAL);
var
  ExecInfo: TShellExecuteInfo;
  NeedUninitialize: Boolean;

begin
  Assert(FileName <> '');

  NeedUninitialize := Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or
    COINIT_DISABLE_OLE1DDE));
  try
    FillChar(ExecInfo, SizeOf(ExecInfo), 0);
    ExecInfo.cbSize := SizeOf(ExecInfo);

    ExecInfo.Wnd := hWindow;
    ExecInfo.lpVerb := Pointer(Operation);
    ExecInfo.lpFile := PChar(FileName);
    ExecInfo.lpParameters := Pointer(Parameters);
    ExecInfo.lpDirectory := Pointer(Directory);
    ExecInfo.nShow := ShowCmd;
    ExecInfo.fMask :=
      //SEE_MASK_NOASYNC { = SEE_MASK_FLAG_DDEWAIT для старых версий Delphi }
    //or
    SEE_MASK_FLAG_NO_UI;
{$IFDEF UNICODE}
    // Не обязательно, см. http://www.transl-gunsmoker.ru/2015/01/what-does-SEEMASKUNICODE-flag-in-ShellExecuteEx-actually-do.html
    ExecInfo.fMask := ExecInfo.fMask or SEE_MASK_UNICODE;
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}
    Win32Check(ShellExecuteEx(@ExecInfo));
{$WARN SYMBOL_PLATFORM ON}
  finally
    if NeedUninitialize then
      CoUninitialize;
  end;
end;

//---------------------------------------------------------------------------

function GetAppPath: AnsiString;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

//---------------------------------------------------------------------------
{ SysInfo Utils }

function GetCPUInfo: string;
const
  KEY_NAME = 'Hardware\Description\System\CentralProcessor\0';
  VALUE_NAME = 'ProcessorNameString';

begin
  Result := '';

  with TRegistry.Create do
    try
      begin
        Access := KEY_READ;
        RootKey := HKEY_LOCAL_MACHINE;

        if OpenKey(KEY_NAME, False) then
        begin
          if ValueExists(VALUE_NAME) then
            Result := Trim(ReadString(VALUE_NAME));
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
end;

//---------------------------------------------------------------------------

function GetCPULevel: string;
var
  SysInfo: TSystemInfo;

begin
  GetSystemInfo(SysInfo);

  case SysInfo.wProcessorLevel of
    3: Result := '80386';
    4: Result := '80486';
    5: Result := 'Pentium';
    6: Result := 'Pentium Pro';
  else
    Result := IntToStr(SysInfo.wProcessorLevel);
  end;
end;

//---------------------------------------------------------------------------

function GetCPUSpeed: Double;
const
  DelayTime = 500;

var
  TimerHi: Dword;
  TimerLo: Dword;
  PriorityClass: Integer;
  Priority: Integer;

begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetThreadPriority(GetCurrentThread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
  Sleep(10);

  asm
    DW 310Fh // rdtsc
    MOV TimerLo, EAX
    MOV TimerHi, EDX
  end;
  Sleep(DelayTime);
  asm
    DW 310Fh // rdtsc
    SUB EAX, TimerLo
    SBB EDX, TimerHi
    MOV TimerLo, EAX
    MOV TimerHi, EDX
  end;

  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  Result := TimerLo / (1000.0 * DelayTime);
end;

//---------------------------------------------------------------------------

function GetPhysicalMemory: Int64;
var
  MemStatus: TMemoryStatus;

begin
  GlobalMemoryStatus(MemStatus);
  Result := MemStatus.dwTotalPhys;
end;

//---------------------------------------------------------------------------

function GetDriveType_Ns(Drive: Char): string;
var
  DriveType: Dword;

begin
  DriveType := GetDriveType(@Drive);

  case DriveType of
    0: Result := '?';
    1: Result := 'Path does not exists';
    Drive_Removable: Result := 'Removable';
    Drive_Fixed: Result := 'Fixed';
    Drive_Remote: Result := 'Remote';
    Drive_CDROM: Result := 'CD-ROM';
    Drive_RamDisk: Result := 'RAMDisk'
  else
    Result := 'Unknown';
  end;
end;

//---------------------------------------------------------------------------

function GetDiskFreeSpaceInfo(Drive: Char; out TotalSize, FreeSize: Int64):
  Boolean;
var
  FDummy: Int64;
  S: string;

begin
  S := Drive + ':';
  Result := GetDiskFreeSpaceEx(PChar(S), FDummy, TotalSize, @FreeSize);
end;

//---------------------------------------------------------------------------

function GetVideoCard: string;
var
  DisplayDevice: TDisplayDevice;
  DeviceNum: Dword;
  dwFlags: Dword;

begin
  DisplayDevice.cb := SizeOf(DisplayDevice);
  DeviceNum := 0;
  dwFlags := 0;

  while EnumDisplayDevices(nil, DeviceNum, DisplayDevice, dwFlags) do
  begin
    Inc(DeviceNum);
    Result := DisplayDevice.DeviceName;
  end;
end;

//---------------------------------------------------------------------------

function GetLANIPAddress: string;
var
  WSAData: TWSAData;
  HostEntry: PHostEnt;
  HostName: array[0..255] of AnsiChar;
  rc: Integer;

begin
  Result := '';

  rc := WSAStartup($0101, WSAData);
  if rc <> 0 then
    Exit;

  GetHostName(HostName, 256);
  HostEntry := GetHostByName(HostName);
  Result := inet_ntoa(PInAddr(HostEntry.h_addr_list^)^);

  WSACleanup;
end;

//---------------------------------------------------------------------------
// http://www.sources.ru/delphi/delphi_get_ip_for_all_interfaces.shtml

function GetInterfaces(var Interfaces: TInterfaces): Boolean;
var
  Soc: TSocket;
  WSAData: TWSAData;
  NumInterfaces: Integer;
  BytesReturned, SetFlags: u_long;
  pAddrInet: SockAddr_In;
  pAddrString: PChar;
  PtrA: Pointer;
  Buffer: array[0..20] of TInterfaceInfo;
  I, rc: Integer;

begin
  Result := False;
  Soc := INVALID_SOCKET;

  rc := WSAStartup($0101, WSAData);
  if rc <> 0 then
    Exit;

  try
    begin
      Soc := Socket(AF_INET, SOCK_STREAM, 0);
      if (Soc = INVALID_SOCKET) then
        Exit;

      try
        PtrA := @BytesReturned;
        if (WSAIoCtl(Soc, I_SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA,
          nil, nil) <> SOCKET_ERROR) then
        begin
          NumInterfaces := BytesReturned div SizeOf(InterfaceInfo);

          SetLength(Interfaces, NumInterfaces * SizeOf(TInterface));

          for i := 0 to NumInterfaces - 1 do
          begin
            pAddrInet := Buffer[i].iiAddress.addressIn;
            pAddrString := inet_ntoa(pAddrInet.sin_addr);
            Interfaces[NumInterfaces].IPAddress := pAddrString;

            pAddrInet := Buffer[i].iiNetMask.addressIn;
            pAddrString := inet_ntoa(pAddrInet.sin_addr);
            Interfaces[NumInterfaces].Mask := pAddrString;

            pAddrInet := Buffer[i].iiBroadCastAddress.addressIn;
            pAddrString := inet_ntoa(pAddrInet.sin_addr);
            Interfaces[NumInterfaces].Broadcast := pAddrString;

            SetFlags := Buffer[i].iiFlags;
            Interfaces[NumInterfaces].IsUp := (SetFlags and I_IFF_UP) =
              I_IFF_UP;
            Interfaces[NumInterfaces].IsBroadcastSupported := (SetFlags and
              I_IFF_BROADCAST) = I_IFF_BROADCAST;
            Interfaces[NumInterfaces].IsLoopback := (SetFlags and I_IFF_LOOPBACK)
              = I_IFF_LOOPBACK;
          end;
        end;
      except
        begin
          //
        end;
      end;

    end;
  finally
    begin
      CloseSocket(Soc);
      WSACleanUp;
    end;
  end;

  Result := True;
end;

//---------------------------------------------------------------------------

function GetKeyboardLanguage: string;
var
  LanguageID: LangID;
  Language: array[0..100] of Char;

begin
  LanguageID := GetSystemDefaultLangID;
  VerLanguageName(LanguageID, Language, 100);
  Result := string(Language);
end;

//---------------------------------------------------------------------------

function GetComputerName_Ns: string;
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  NameSize: Cardinal;

begin
  NameSize := MAX_COMPUTERNAME_LENGTH + 1;
  if not GetComputerName(Buffer, NameSize) then
    RaiseLastOSError;
  Result := Buffer;
end;

//---------------------------------------------------------------------------

function GetOSRegInfo(var RegOwner: string; var RegOrg: string): Integer;
const
  WIN95_KEY = '\SOFTWARE\Microsoft\Windows\CurrentVersion';
  WINNT_KEY = '\SOFTWARE\Microsoft\Windows NT\CurrentVersion';

var
  VersionKey: PChar;

begin
  Result := 0;

  if GetPlatformName = 'Win95' then
    VersionKey := WIN95_KEY
  else if GetPlatformName = 'WinNT' then
    VersionKey := WINNT_KEY
  else
  begin
    Result := -1;
    Exit;
  end;

  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(VersionKey, False) then
      begin
        RegOwner := ReadString('RegisteredOwner');
        RegOrg := ReadString('RegisteredOrganization');
      end;
    finally
      Free;
    end;
end;

//---------------------------------------------------------------------------

function GetWindowsUserName: string;
var
  UserName: string;
  UserNameLen: Dword;
begin
  UserNameLen := 255;
  SetLength(userName, UserNameLen);

  if GetUserName(PChar(UserName), UserNameLen) then
    Result := Copy(UserName, 1, UserNameLen - 1)
  else
    Result := 'Unknown';
end;

//---------------------------------------------------------------------------

procedure GetUserNameEx(NameFormat: Dword; lpNameBuffer: lpStr; nSize: puLong);
  stdcall;
  external 'secur32.dll' Name 'GetUserNameExA';

function GetLoggedOnUserName(UserFormat: TUserFormat): string;
var
  UserName: array[0..250] of Char;
  NameSize: Dword;
begin
  NameSize := 250;
  GetUserNameEx(Cardinal(UserFormat), @UserName, @NameSize);
  Result := UserName;
end;

//---------------------------------------------------------------------------

function GetPlatformName: string;
var
  VersionInfo: TOSVersionInfo;

begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  case VersionInfo.dwPlatformId of
    Ver_Platform_Win32s: Result := 'Win32s';
    Ver_Platform_Win32_Windows: Result := 'Win95';
    Ver_Platform_Win32_NT: Result := 'WinNT'
  else
    Result := 'Unknown Platform';
  end;
end;

//---------------------------------------------------------------------------

function GetWindowsVersion: string;
type
  TWinVersion = (wvUnknown, wv95, wv98, wvME, wvNT3, wvNT4, wvW2K, wvXP,
    wv2003, wvVista);

const
  FWinVersionStr: array[TWinVersion] of string = ('', 'WIN_95', 'WIN_98',
    'WIN_ME', 'WIN_NT3', 'WIN_NT4', 'WIN_2000', 'WIN_XP', 'WIN_2003',
    'WIN_VISTA');

var
  WinVersion: TWinVersion;
  VersionInfo: TOSVersionInfo;
  ServicePack: string;

begin
  WinVersion := wvUnknown;
  ServicePack := '';
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);

  if GetVersionEx(VersionInfo) then
  begin
    case VersionInfo.DwMajorVersion of
      3: WinVersion := wvNT3;
      4:
        case VersionInfo.DwMinorVersion of
          0:
            if VersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
              WinVersion := wvNT4
            else
              WinVersion := wv95;
          10: WinVersion := wv98;
          90: WinVersion := wvME;
        end;
      5:
        case VersionInfo.DwMinorVersion of
          0: WinVersion := wvW2K;
          1: WinVersion := wvXP;
          2: WinVersion := wv2003;
        end;
      6:
        case VersionInfo.DwMinorVersion of
          0: WinVersion := wvVista;
        end;
    end;
    ServicePack := Trim(VersionInfo.szCSDVersion);
  end;

  if WinVersion <> wvUnknown then
    Result := FWinVersionStr[WinVersion] + ' ' + ServicePack
  else
    Result := Format('%d_%d', [VersionInfo.DwMajorVersion,
      VersionInfo.DwMinorVersion]);
end;

//---------------------------------------------------------------------------

function GetOSVersionInfo(var OSVersion: TOSVersion): string;
var
  VersionInfo: TOSVersionInfo;

begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  OSVersion.MajorVersion := VersionInfo.dwMajorVersion;
  OSVersion.MinorVersion := VersionInfo.dwMinorVersion;
  OSVersion.BuildNumber := LoWord(VersionInfo.dwBuildNumber);
  OSVersion.CSDVersion := VersionInfo.szCSDVersion;
  case VersionInfo.dwPlatformId of
    Ver_Platform_Win32s: OSVersion.PlatformName := 'Win32s';
    Ver_Platform_Win32_Windows: OSVersion.PlatformName := 'Win95';
    Ver_Platform_Win32_NT: OSVersion.PlatformName := 'WinNT'
  else
    OSVersion.PlatformName := 'Unknown Platform';
  end;

  Result := Format('%s version %d.%d build %d %s',
    [OSVersion.PlatformName, OSVersion.MajorVersion, OSVersion.MinorVersion,
    OSVersion.BuildNumber, OSVersion.CSDVersion]);
end;

//---------------------------------------------------------------------------

function GetProcessesWin95(var Processes: TProcArray): Integer;
var
  FSnap: THandle;
  ProcEntry: TProcessEntry32;
  PProcEntry: PProcessEntry32;
  I: Integer;

begin
  if FSnap > 0 then
    CloseHandle(FSnap);

  FSnap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  ProcEntry.dwSize := SizeOf(ProcEntry);
  I := 0;
  SetLength(Processes, $3FFF - 1);

  if Process32First(FSnap, ProcEntry) then
    repeat
      New(PProcEntry);
      PProcEntry^ := ProcEntry;
      Processes[I] := PProcEntry.szExeFile;
      I := I + 1;
    until not Process32Next(FSnap, ProcEntry);

  Result := I;
  if FSnap > 0 then
    CloseHandle(FSnap);
end;

//---------------------------------------------------------------------------

function GetProcessesWinNT(var Processes: TProcArray): Integer;
var
  Num: Integer;
  PIDs: array[0..$3FFF - 1] of Dword;
  BufferSize: Dword;
  BufferRealSize: Dword;
  ProcHandle: THandle;
  ModuleHandle: HModule;
  ModuleName: array[0..MAX_PATH] of Char;
  I: Integer;

begin
  ProcHandle := 0;
  ModuleHandle := 0;
  BufferSize := 0;

  EnumProcesses(@PIDs, BufferSize, BufferRealSize);
  Num := BufferRealSize div SizeOf(Dword);
  SetLength(Processes, Num);

  for I := 0 to Num - 1 do
  begin
    ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
      False, PIDs[I]);
    if GetModuleFileNameEx(ProcHandle, ModuleHandle, ModuleName,
      SizeOf(ModuleName)) > 0 then
      Processes[I] := ModuleName
    else
      Processes[I] := 'Unknown';
  end;

  if ProcHandle > 0 then
    CloseHandle(ProcHandle);
  Result := Num;
end;

//---------------------------------------------------------------------------

function GetWindowsDir: string;
var
  S: array[0..MAX_PATH] of Char;

begin
  GetWindowsDirectory(S, SizeOf(S));
  Result := S;
end;

//---------------------------------------------------------------------------

function GetSystemDir: string;
var
  S: array[0..MAX_PATH] of Char;

begin
  GetSystemDirectory(S, SizeOf(S));
  Result := S;
end;

//---------------------------------------------------------------------------

function GetCurrentDir: string;
var
  S: array[0..MAX_PATH] of Char;

begin
  GetCurrentDirectory(SizeOf(S), S);
  Result := S;
end;

//---------------------------------------------------------------------------
{ File Utils }

function GetFileInfo(const AFileName: string; out FileSize: Integer;
  out FileTime: Int64): Boolean;
var
  F: TSearchRec;

begin
  Result := FindFirst(AFileName, 0, F) = 0;

  if Result then
    try
      FileSize := F.Size;
      Int64Rec(FileTime).Lo := F.FindData.ftLastWriteTime.dwLowDateTime;
      Int64Rec(FileTime).Hi := F.FindData.ftLastWriteTime.dwHighDateTime;
    finally
      SysUtils.FindClose(F);
    end;
end;

//---------------------------------------------------------------------------

function GetFileVersion(const FileName: string;
  out MajorVersion, MinorVersion: Integer): Boolean;
var
  Info: Pointer;
  InfoSize: Cardinal;
  Dummy: Cardinal;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: Cardinal;

begin
  Result := False;
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);

  if InfoSize > 0 then
  begin
    GetMem(Info, InfoSize);
    try
      if not GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info) then
        RaiseLastOSError;

      if VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize) then
      begin
        MajorVersion := FileInfo.dwFileVersionMS shr 16;
        MinorVersion := FileInfo.dwFileVersionMS and $FFFF;
        Result := True;
      end;
    finally
      FreeMem(Info);
    end;
  end;
end;

//--------------------------------------------------------------------------

function GetFullFileVersion(const FileName: string): string;
var
  InfoSize, puLen: Dword;
  Info, InfoPtr: Pointer;
  FileInfo: TVSFixedFileInfo;
begin
  InfoSize := GetFileVersionInfoSize(PChar(FileName), puLen);

  if InfoSize > 0 then
  begin
    GetMem(Info, InfoSize);
    try
      if not GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info) then
        RaiseLastOSError;

      FillChar(FileInfo, SizeOf(TVSFixedFileInfo), 0);

      VerQueryValue(Info, '\', InfoPtr, puLen);
      Move(InfoPtr^, FileInfo, SizeOf(TVSFixedFileInfo));
    finally
      FreeMem(Info);
    end;

    Result := Format('%u.%u.%u.%u', [HiWord(FileInfo.dwProductVersionMS),
      LoWord(FileInfo.dwProductVersionMS),
        HiWord(FileInfo.dwProductVersionLS),
        LoWord(FileInfo.dwProductVersionLS)]);
  end
  else
    Result := '';
end;

//---------------------------------------------------------------------------

function GetFullPathOfFile(const FileName: string): string;
const
  KB = 1024;

var
  Buffer: array[0..KB] of Char;
  FilePart: PChar;

begin
  if GetFullPathName(PChar(FileName), KB, Buffer, FilePart) = 0 then
    RaiseLastOSError;
  SetString(Result, Buffer, FilePart - Buffer);
end;

//---------------------------------------------------------------------------
{ CRC }

function CRCMakerPas(InputByte, CRCByte: Byte): Byte;
var
  C: Byte;
  W: Word;
  I: Integer;

begin
  W := InputByte + CRCByte shl 8;

  for I := 0 to 7 do
  begin
    C := W and $8000;
    W := W shl 1;
    if C <> 0 then
      W := W xor $6900;
  end;

  Result := W shr 8;
end;

//---------------------------------------------------------------------------

function CRCMakerAsm(InputByte, CRCByte: Byte): Byte;
begin
  Result := 0;
  {  asm
      mov al,InputByte
      mov ah,CRCByte
      mov cx,8
    mod1:
      rol al,1
      rcl ah,1
      jnc mod2
      xor ah,69h
    mod2:
      dec cx
      jnz mod1
      mov al,ah
    end;
    }
end;

//---------------------------------------------------------------------------
{ Convert }

function StringToArray(Str: string): TCharArray;
begin
  FillChar(Result, High(Byte), #0);
  Move(Str[1], Result, Length(Str));
end;

//---------------------------------------------------------------------------

function StringToDynArray(Str: string): TCharDynArray;
begin
  SetLength(Result, Length(Str));
  Move(Str[1], Result[0], Length(Str));
end;

//---------------------------------------------------------------------------

function DefCurrency(Value: Variant): Currency;
const
  S_DOT: AnsiString = '.';
  S_COMMA: AnsiString = ',';
  S_SPACE: AnsiString = ' ';
  S_NOTHING: AnsiString = '';
  NUMBERS = [#48..#58]; // Numbers from 0 to 9

var
  I: Integer;
  DSExists: Boolean;
  StrValue: AnsiString;

begin
  try
    Result := StrToFloat(Value)
  except
    try
      StrValue := Value;
      // Replace separators

{      if AnsiPos(S_DOT, StrValue) <> 0 then
        Value := StringReplace(StrValue, S_DOT, DecimalSeparator, [rfReplaceAll]);
 }     if AnsiPos(S_COMMA, StrValue) <> 0 then
        Value := StringReplace(StrValue, S_COMMA, '.', [rfReplaceAll]);
      if AnsiPos(S_SPACE, StrValue) <> 0 then
        Value := StringReplace(StrValue, S_SPACE, S_NOTHING, [rfReplaceAll]);

      // Check for non-numeric symbols and separators count
      DSExists := False;
      for I := 1 to Length(StrValue) do
      begin
        // Non-numeric symbols
        if not(StrValue[I] in NUMBERS) and (StrValue[I] <> DecimalSeparator) then
          Break;
        // Separators count
        if StrValue[I] = DecimalSeparator then
          if not DSExists then
            DSExists := True
          else
            Break;
      end;
      Result := StrToFloat(StrValue);
    except
      Result := 0;
    end;
  end;
end;

//---------------------------------------------------------------------------

function DefInteger(Value: Variant): Integer;
begin
  try
    Result := StrToInt(Value)
  except
    Result := 0;
  end;
end;

//---------------------------------------------------------------------------

function BoolToInt(Value: Boolean): Integer;
begin
  Result := 0;
  if Value then Result := 1;
end;

//---------------------------------------------------------------------------

function StrToBoolean(S: String): Boolean;
begin
  Result := False;
  if UpperCase(S) = 'ДА' then Result := True;
  if UpperCase(S) = 'НЕТ' then Result := False;
end;

function ExtendLeft(Str: String; NewLen: Integer; AddStr: String): String;
begin
  while Length(Str) < NewLen do
    Str := AddStr + Str;

  Result := Str;
end;

end.

