unit TaurusTLS.UT.TestClasses;

interface

uses
  System.SysUtils, DUnitX.TestFramework, TaurusTLS.UT.Utils;

type
  EOsslBaseFixture = class(Exception);
  [TestFixture]
  TOsslBaseFixture = class
  public type
    TTestFlag = (tfOssLibLoaded, tfOssLibFailed, tfGlobalFail, tfBoundary=SizeOf(Cardinal)*8-1);
    TTestFlags = set of TTestFlag;
  private class var
    FFlags: TTestFlags;
  private
    class function GetGlobalFail: boolean; static;
    class function GetOssLibFailed: boolean; static;
    class function GetOssLibLoaded: boolean; static;
    class procedure SetOssLibLoaded(const Value: boolean); static;
    class procedure SetGlobalFail(const Value: boolean); static;
    class procedure SetOssLibFailed(const Value: boolean); static;
    class procedure SetFlag(const AFlag: TTestFlag; AEnable: boolean); static;
  public
    [SetupFixture]
    procedure SetupFixture; virtual;
    [TearDownFixture]
    procedure TearDownFixture; virtual;

    class procedure CheckLoaded;
    class procedure CheckFailed; virtual;
    class procedure CheckAllFailures; virtual;

    class property OssLibLoaded: boolean read GetOssLibLoaded write SetOssLibLoaded;
    class property OssLibFailed: boolean read GetOssLibFailed write SetOssLibFailed;
    class property GlobalFail: boolean read GetGlobalFail write SetGlobalFail;
  end;

resourcestring
  cOsslNotLoaded = 'Openssl library is not loaded. Skipping remaining tests(s)';
  cOsslLoadFailed = 'Openssl library load failed. Skipping remaining tests(s)';
  cOsslGlobalFail = 'Openssl global failure. Skipping remaining tests(s)';

implementation

uses
  System.SyncObjs;

{ TOsslTestBase }

class procedure TOsslBaseFixture.CheckAllFailures;
begin
  CheckLoaded;
  CheckFailed;
end;

class procedure TOsslBaseFixture.CheckFailed;
var
  lReason: string;

begin
  lReason:='';
  if GlobalFail then
    lReason:=cOsslGlobalFail
  else if OssLibFailed then
    lReason:=cOsslLoadFailed;
  if not lReason.IsEmpty then
    raise EOsslBaseFixture.Create(lReason);
end;

class procedure TOsslBaseFixture.CheckLoaded;
begin
  if not OssLibLoaded then
    raise EOsslBaseFixture.CreateRes(@cOsslNotLoaded);
end;

class function TOsslBaseFixture.GetGlobalFail: boolean;
begin
  Result:=tfGlobalFail in FFlags;
end;

class function TOsslBaseFixture.GetOssLibFailed: boolean;
begin
  Result:=tfOssLibFailed in FFlags;
end;

class function TOsslBaseFixture.GetOssLibLoaded: boolean;
begin
  Result:=tfOssLibLoaded in FFlags;
end;

class procedure TOsslBaseFixture.SetOssLibLoaded(const Value: boolean);
begin
  SetFlag(tfOssLibLoaded, Value);
end;

class procedure TOsslBaseFixture.SetGlobalFail(const Value: boolean);
begin
  SetFlag(tfGlobalFail, Value);
end;

class procedure TOsslBaseFixture.SetOssLibFailed(const Value: boolean);
begin
  SetFlag(tfOssLibFailed, Value);
  GlobalFail:=True;
end;

class procedure TOsslBaseFixture.SetFlag(const AFlag: TTestFlag; AEnable: boolean);
var
  lSpinW: TSpinWait;
  lFlags: TTestFlags;
  lNewFlags: TTestFlags;

begin
  lSpinW.Reset;
  repeat
    lFlags:=FFlags;
    lNewFlags:=lFlags;
    if AEnable then
      lNewFlags:=lFlags+[AFlag]
    else
      lNewFlags:=lFlags-[AFlag];
    // Hack for SET Varibale atomic update
    cardinal((@lFlags)^):=TInterlocked.CompareExchange(cardinal((@FFlags)^),
      cardinal((@lNewFlags)^), cardinal((@lFlags)^));
    if ((AFlag in FFlags) xor (not AEnable)) then
      break;
    lSpinW.SpinCycle;
  until False;
end;

procedure TOsslBaseFixture.SetupFixture;
{$IFDEF MSWINDOWS}
  {$IFNDEF MEMLEAK_DETAILS}
var
  lDebugActive: boolean;
  {$ENDIF MEMLEAK_DETAILS}
{$ENDIF}

begin
{$IFDEF MSWINDOWS}
  {$IFDEF MEMLEAK_DETAILS}
  TFastMMDebugLog.DebugModeActive:=True;
  {$ELSE}
  lDebugActive:=TFastMMDebugLog.DebugModeActive; // To enforce linker add the TFastMMDebugLog class
  {$ENDIF MEMLEAK_DETAILS}
{$ENDIF}
  try
    OssLibLoaded:=TOsslLoader.Load;
    CheckAllFailures;
  except
    GlobalFail:=True;
    raise
  end;
end;

procedure TOsslBaseFixture.TearDownFixture;
begin
  if OssLibLoaded then
  try
    TOsslLoader.Unload;
  except
    GlobalFail:=True;
    CheckFailed;
    raise;
  end;
end;

initialization

end.
