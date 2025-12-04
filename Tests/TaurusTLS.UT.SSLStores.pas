unit TaurusTLS.UT.SSLStores;

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Rtti,
  DUnitX.TestFramework,
  IdGlobal,
  IdCTypes,
  TaurusTLS_SSLStores;

const
  cFlagsDelim = ';';

type
  [TestFixture]
  TX509FvyParamVerifyFlagsFixture = class
  public type
    TVerifyFlag = TaurusTLS_CustomX509VerifyParam.TVerifyFlag;
    TVerifyFlags = TaurusTLS_CustomX509VerifyParam.TVerifyFlags;

  private const
    cNames: array[TVerifyFlag] of string =
    ('x509vfCheckTime', 'x509vfCheckCrl', 'x509vfCheckCrlAll', 'x509vfIgnoreCritical',
     'x509vfStrict', 'x509vfAllowProxyCerts', 'x509vfPolicyCheck', 'x509vfExplicitPolicy',
     'x509vfInhibitAny', 'x509vfInhibitMap', 'x509vfNotifyPolicy', 'x509vfExtendCrlSupport',
     'x509vfUseCrlDeltas', 'x509vfCheckSelfSignSignitures', 'x509vfTrustedFirst',
     'x509vfSuiteB128Only', 'x509vfSuiteB192', '' {empty item = #18}, 'x509vfPartialChain',
     'x509vfNoAlternativeChain', 'x509vfNoCheckTime');

    cNamesAll = 'x509vfCheckTime;x509vfCheckCrl;x509vfCheckCrlAll;x509vfIgnoreCritical;'+
     'x509vfStrict;x509vfAllowProxyCerts;x509vfPolicyCheck;x509vfExplicitPolicy;'+
     'x509vfInhibitAny;x509vfInhibitMap;x509vfNotifyPolicy;x509vfExtendCrlSupport;'+
     'x509vfUseCrlDeltas;x509vfCheckSelfSignSignitures;x509vfTrustedFirst;'+
     'x509vfSuiteB128Only;x509vfSuiteB192;x509vfPartialChain;x509vfNoAlternativeChain;'+
     'x509vfNoCheckTime';

  protected
    function GetFlagValue(AName: string): TVerifyFlag;
    function GetFlagName(Value: TVerifyFlag): string;
    function GetIntValue(Value: TVerifyFlag): integer;
    function GetFlagsFromString(ANames: string): TVerifyFlags;

  public
    [AutoNameTestCase('$000002,x509vfCheckTime')]
    [AutoNameTestCase('$000004,x509vfCheckCrl')]
    [AutoNameTestCase('$000008,x509vfCheckCrlAll')]
    [AutoNameTestCase('$000010,x509vfIgnoreCritical')]
    [AutoNameTestCase('$000020,x509vfStrict')]
    [AutoNameTestCase('$000040,x509vfAllowProxyCerts')]
    [AutoNameTestCase('$000080,x509vfPolicyCheck')]
    [AutoNameTestCase('$000100,x509vfExplicitPolicy')]
    [AutoNameTestCase('$000200,x509vfInhibitAny')]
    [AutoNameTestCase('$000400,x509vfInhibitMap')]
    [AutoNameTestCase('$000800,x509vfNotifyPolicy')]
    [AutoNameTestCase('$001000,x509vfExtendCrlSupport')]
    [AutoNameTestCase('$002000,x509vfUseCrlDeltas')]
    [AutoNameTestCase('$004000,x509vfCheckSelfSignSignitures')]
    [AutoNameTestCase('$008000,x509vfTrustedFirst')]
    [AutoNameTestCase('$010000,x509vfSuiteB128Only')]
    [AutoNameTestCase('$020000,x509vfSuiteB192')]
    [AutoNameTestCase('$080000,x509vfPartialChain')]
    [AutoNameTestCase('$100000,x509vfNoAlternativeChain')]
    [AutoNameTestCase('$200000,x509vfNoCheckTime')]
    procedure PositiveFlagAsIntSet(AIntVal: TIdC_ULONG; AFlag: string);

    [AutoNameTestCase('x509vfCheckTime,$000002')]
    [AutoNameTestCase('x509vfCheckCrl,$000004')]
    [AutoNameTestCase('x509vfCheckCrlAll,$000008')]
    [AutoNameTestCase('x509vfIgnoreCritical,$000010')]
    [AutoNameTestCase('x509vfStrict,$000020')]
    [AutoNameTestCase('x509vfAllowProxyCerts,$000040')]
    [AutoNameTestCase('x509vfPolicyCheck,$000080')]
    [AutoNameTestCase('x509vfExplicitPolicy,$000100')]
    [AutoNameTestCase('x509vfInhibitAny,$000200')]
    [AutoNameTestCase('x509vfInhibitMap,$000400')]
    [AutoNameTestCase('x509vfNotifyPolicy,$000800')]
    [AutoNameTestCase('x509vfExtendCrlSupport,$001000')]
    [AutoNameTestCase('x509vfUseCrlDeltas,$002000')]
    [AutoNameTestCase('x509vfCheckSelfSignSignitures,$004000')]
    [AutoNameTestCase('x509vfTrustedFirst,$008000')]
    [AutoNameTestCase('x509vfSuiteB128Only,$010000')]
    [AutoNameTestCase('x509vfSuiteB192,$020000')]
    [AutoNameTestCase('x509vfPartialChain,$080000')]
    [AutoNameTestCase('x509vfNoAlternativeChain,$100000')]
    [AutoNameTestCase('x509vfNoCheckTime,$200000')]
    procedure PositiveFlagAsIntGet(AFlag: string; AIntVal: TIdC_ULONG);

    [AutoNameTestCase('$000000')]
    [AutoNameTestCase('$000001')]
    [AutoNameTestCase('$000003')]
    [AutoNameTestCase('$040000')]
    [AutoNameTestCase('$400000')]
    [AutoNameTestCase('$FFFFFFFF')]
    procedure NegativeFlagAsIntSet(AIntVal: TIdC_ULONG);

    [AutoNameTestCase('x509vfCheckTime,$000002')]
    [AutoNameTestCase('x509vfCheckCrl,$000004')]
    [AutoNameTestCase('x509vfCheckCrlAll,$000008')]
    [AutoNameTestCase('x509vfIgnoreCritical,$000010')]
    [AutoNameTestCase('x509vfStrict,$000020')]
    [AutoNameTestCase('x509vfAllowProxyCerts,$000040')]
    [AutoNameTestCase('x509vfPolicyCheck,$000080')]
    [AutoNameTestCase('x509vfExplicitPolicy,$000100')]
    [AutoNameTestCase('x509vfInhibitAny,$000200')]
    [AutoNameTestCase('x509vfInhibitMap,$000400')]
    [AutoNameTestCase('x509vfNotifyPolicy,$000800')]
    [AutoNameTestCase('x509vfExtendCrlSupport,$001000')]
    [AutoNameTestCase('x509vfUseCrlDeltas,$002000')]
    [AutoNameTestCase('x509vfCheckSelfSignSignitures,$004000')]
    [AutoNameTestCase('x509vfTrustedFirst,$008000')]
    [AutoNameTestCase('x509vfSuiteB128Only,$010000')]
    [AutoNameTestCase('x509vfSuiteB192,$020000')]
    [AutoNameTestCase('x509vfPartialChain,$080000')]
    [AutoNameTestCase('x509vfNoAlternativeChain,$100000')]
    [AutoNameTestCase('x509vfNoCheckTime,$200000')]
    procedure FlagIsEqualTo(AFlag: string; AIntVal: TIdC_ULONG);

    [AutoNameTestCase(',$000000')]
    [AutoNameTestCase('x509vfCheckTime,$000002')]
    [AutoNameTestCase('x509vfCheckCrl,$000004')]
    [AutoNameTestCase('x509vfCheckCrlAll,$000008')]
    [AutoNameTestCase('x509vfIgnoreCritical,$000010')]
    [AutoNameTestCase('x509vfStrict,$000020')]
    [AutoNameTestCase('x509vfAllowProxyCerts,$000040')]
    [AutoNameTestCase('x509vfPolicyCheck,$000080')]
    [AutoNameTestCase('x509vfExplicitPolicy,$000100')]
    [AutoNameTestCase('x509vfInhibitAny,$000200')]
    [AutoNameTestCase('x509vfInhibitMap,$000400')]
    [AutoNameTestCase('x509vfNotifyPolicy,$000800')]
    [AutoNameTestCase('x509vfExtendCrlSupport,$001000')]
    [AutoNameTestCase('x509vfUseCrlDeltas,$002000')]
    [AutoNameTestCase('x509vfCheckSelfSignSignitures,$004000')]
    [AutoNameTestCase('x509vfTrustedFirst,$008000')]
    [AutoNameTestCase('x509vfSuiteB128Only,$010000')]
    [AutoNameTestCase('x509vfSuiteB192,$020000')]
    [AutoNameTestCase('x509vfPartialChain,$080000')]
    [AutoNameTestCase('x509vfNoAlternativeChain,$100000')]
    [AutoNameTestCase('x509vfNoCheckTime,$200000')]
    [AutoNameTestCase('x509vfCheckTime;x509vfPartialChain,$080002')]
    [AutoNameTestCase(cNamesAll+',$3BFFFE')]
    procedure PositiveFlagsAsIntGet(AFlags: string; AIntVal: TIdC_ULONG);

    [AutoNameTestCase('$000000,')]
    [AutoNameTestCase('$000002,x509vfCheckTime')]
    [AutoNameTestCase('$000004,x509vfCheckCrl')]
    [AutoNameTestCase('$000008,x509vfCheckCrlAll')]
    [AutoNameTestCase('$000010,x509vfIgnoreCritical')]
    [AutoNameTestCase('$000020,x509vfStrict')]
    [AutoNameTestCase('$000040,x509vfAllowProxyCerts')]
    [AutoNameTestCase('$000080,x509vfPolicyCheck')]
    [AutoNameTestCase('$000100,x509vfExplicitPolicy')]
    [AutoNameTestCase('$000200,x509vfInhibitAny')]
    [AutoNameTestCase('$000400,x509vfInhibitMap')]
    [AutoNameTestCase('$000800,x509vfNotifyPolicy')]
    [AutoNameTestCase('$001000,x509vfExtendCrlSupport')]
    [AutoNameTestCase('$002000,x509vfUseCrlDeltas')]
    [AutoNameTestCase('$004000,x509vfCheckSelfSignSignitures')]
    [AutoNameTestCase('$008000,x509vfTrustedFirst')]
    [AutoNameTestCase('$010000,x509vfSuiteB128Only')]
    [AutoNameTestCase('$020000,x509vfSuiteB192')]
    [AutoNameTestCase('$080000,x509vfPartialChain')]
    [AutoNameTestCase('$100000,x509vfNoAlternativeChain')]
    [AutoNameTestCase('$200000,x509vfNoCheckTime')]
    [AutoNameTestCase('$080002,x509vfCheckTime;x509vfPartialChain')]
    [AutoNameTestCase('$3BFFFE,'+cNamesAll)]
    procedure PositiveFlagsAsIntSet(AIntVal: TIdC_ULONG; AFlags: string);

    [AutoNameTestCase('$3FFFFF,'+cNamesAll)]
    [AutoNameTestCase('$FFFFFF,'+cNamesAll)]
    [AutoNameTestCase('$020000,x509vfSuiteB192')]
    [AutoNameTestCase('$080000,x509vfPartialChain')]
    procedure PositiveFlagsSafeAsIntSet(AIntVal: TIdC_ULONG; AFlags: string);

    [AutoNameTestCase('$3BFFFF')]
    [AutoNameTestCase('$040000')]
    [AutoNameTestCase('$FFFFFFFF')]
    procedure NegativeFlagsAsIntSet(AIntVal: TIdC_ULONG);

    [AutoNameTestCase('x509vfCheckTime;x509vfPartialChain,$80002')]
    [AutoNameTestCase(cNamesAll+',$3BFFFE')]
    procedure FlagsIsEqualTo(AFlags: string; AIntVal: TIdC_ULONG);
  end;

  [TestFixture]
  TX509FvyParamInteritanceFlagsFixture = class
  public type
    TInheritanceFlag = TaurusTLS_CustomX509VerifyParam.TInheritanceFlag;
    TInheritanceFlags = TaurusTLS_CustomX509VerifyParam.TInheritanceFlags;
  protected
    function GetFlagValue(AName: string): TInheritanceFlag;
    function GetFlagName(Value: TInheritanceFlag): string;
    function GetIntValue(Value: TInheritanceFlag): integer;
    function GetFlagsFromString(ANames: string): TInheritanceFlags;

  public
    [AutoNameTestCase('$01,x509ihfDefault')]
    [AutoNameTestCase('$02,x509ihfOverrite')]
    [AutoNameTestCase('$04,x509ihfReset')]
    [AutoNameTestCase('$08,x509ihfLocked')]
    [AutoNameTestCase('$10,x509ihfOnce')]
    procedure PositiveFlagAsIntSet(AIntVal: TIdC_UINT32; AFlag: TInheritanceFlag);

    [AutoNameTestCase('x509ihfDefault,$01')]
    [AutoNameTestCase('x509ihfOverrite,$02')]
    [AutoNameTestCase('x509ihfReset,$04')]
    [AutoNameTestCase('x509ihfLocked,$08')]
    [AutoNameTestCase('x509ihfOnce,$10')]
    procedure PositiveFlagAsIntGet(AFlag: string; AIntVal: TIdC_UINT32);

    [AutoNameTestCase('$000000')]
    [AutoNameTestCase('$000003')]
    [AutoNameTestCase('$000400')]
    [AutoNameTestCase('$400002')]
    [AutoNameTestCase('$FFFFFF')]
    [AutoNameTestCase('$FFFFFFFF')]
    procedure NegativeFlagAsIntSet(AIntVal: TIdC_UINT32; AFlag: TInheritanceFlag);

    [AutoNameTestCase('x509ihfDefault,$01')]
    [AutoNameTestCase('x509ihfOverrite,$02')]
    [AutoNameTestCase('x509ihfReset,$04')]
    [AutoNameTestCase('x509ihfLocked,$08')]
    [AutoNameTestCase('x509ihfOnce,$10')]
    procedure FlagIsEqualTo(AFlag: TInheritanceFlag; AIntVal: TIdC_UINT32);

    [AutoNameTestCase('$00,')]
    [AutoNameTestCase('$01,x509ihfDefault')]
    [AutoNameTestCase('$02,x509ihfOverrite')]
    [AutoNameTestCase('$04,x509ihfReset')]
    [AutoNameTestCase('$08,x509ihfLocked')]
    [AutoNameTestCase('$10,x509ihfOnce')]
    procedure PositiveFlagsAsIntSet(AIntVal: TIdC_UINT32; AFlags: string);

    [AutoNameTestCase(',$00')]
    [AutoNameTestCase('x509ihfDefault,$01')]
    [AutoNameTestCase('x509ihfOverrite,$02')]
    [AutoNameTestCase('x509ihfReset,$04')]
    [AutoNameTestCase('x509ihfLocked,$08')]
    [AutoNameTestCase('x509ihfOnce,$10')]
    [AutoNameTestCase('x509ihfDefault;x509ihfReset;x509ihfOnce,$15')]
    [AutoNameTestCase('x509ihfDefault;x509ihfOverrite;x509ihfReset;x509ihfLocked;x509ihfOnce,$1F')]
    procedure PositiveFlagsAsIntGet(AFlags: string; AIntVal: TIdC_UINT32);

    [AutoNameTestCase('$20')]
    [AutoNameTestCase('$FFFFFFFF')]
    procedure NegativeFlagsAsIntSet(AIntVal: TIdC_UINT32);

    [AutoNameTestCase('x509ihfDefault,$01')]
    [AutoNameTestCase('x509ihfOverrite,$02')]
    [AutoNameTestCase('x509ihfReset,$04')]
    [AutoNameTestCase('x509ihfLocked,$08')]
    [AutoNameTestCase('x509ihfOnce,$10')]
    [AutoNameTestCase('x509ihfDefault;x509ihfReset;x509ihfOnce,$15')]
    [AutoNameTestCase('x509ihfDefault;x509ihfOverrite;x509ihfReset;x509ihfLocked;x509ihfOnce,$1F')]
    procedure FlagsIsEqualTo(AFlags: string; AIntVal: TIdC_UINT32);
  end;

  [TestFixture]
  TX509FvyParamHostCheckFlagsFixture = class
  public type
    THostCheckFlag = TaurusTLS_CustomX509VerifyParam.THostCheckFlag;
    THostCheckFlags = TaurusTLS_CustomX509VerifyParam.THostCheckFlags;
  protected
    function GetFlagValue(AName: string): THostCheckFlag;
    function GetFlagName(Value: THostCheckFlag): string;
    function GetIntValue(Value: THostCheckFlag): integer;
    function GetFlagsFromString(ANames: string): THostCheckFlags;

  public
    [AutoNameTestCase('$01,hckAlwaysChkSubj')]
    [AutoNameTestCase('$02,hckNoWildcard')]
    [AutoNameTestCase('$04,hckNoPartWildcard')]
    [AutoNameTestCase('$08,hckMultiLblWildcard')]
    [AutoNameTestCase('$10,hckSingleLblSubDomain')]
    procedure PositiveFlagAsIntSet(AIntVal: TIdC_UINT; AFlag: THostCheckFlag);

    [AutoNameTestCase('hckAlwaysChkSubj,$01')]
    [AutoNameTestCase('hckNoWildcard,$02')]
    [AutoNameTestCase('hckNoPartWildcard,$04')]
    [AutoNameTestCase('hckMultiLblWildcard,$08')]
    [AutoNameTestCase('hckSingleLblSubDomain,$10')]
    procedure PositiveFlagAsIntGet(AFlag: string; AIntVal: TIdC_UINT);

    [AutoNameTestCase('$000000')]
    [AutoNameTestCase('$000003')]
    [AutoNameTestCase('$000400')]
    [AutoNameTestCase('$400002')]
    [AutoNameTestCase('$FFFFFF')]
    [AutoNameTestCase('$FFFFFFFF')]
    procedure NegativeFlagAsIntSet(AIntVal: TIdC_UINT; AFlag: THostCheckFlag);

    [AutoNameTestCase('hckAlwaysChkSubj,$01')]
    [AutoNameTestCase('hckNoWildcard,$02')]
    [AutoNameTestCase('hckNoPartWildcard,$04')]
    [AutoNameTestCase('hckMultiLblWildcard,$08')]
    [AutoNameTestCase('hckSingleLblSubDomain,$10')]
    procedure FlagIsEqualTo(AFlag: THostCheckFlag; AIntVal: TIdC_UINT32);

    [AutoNameTestCase('$00,')]
    [AutoNameTestCase('$01,hckAlwaysChkSubj')]
    [AutoNameTestCase('$02,hckNoWildcard')]
    [AutoNameTestCase('$04,hckNoPartWildcard')]
    [AutoNameTestCase('$08,hckMultiLblWildcard')]
    [AutoNameTestCase('$10,hckSingleLblSubDomain')]
    [AutoNameTestCase('$15,hckAlwaysChkSubj;hckNoPartWildcard;hckSingleLblSubDomain')]
    [AutoNameTestCase('$1F,hckAlwaysChkSubj;hckNoWildcard;hckNoPartWildcard;'+
      'hckMultiLblWildcard;hckSingleLblSubDomain')]
    procedure PositiveFlagsAsIntSet(AIntVal: TIdC_UINT; AFlags: string);

    [AutoNameTestCase(',$00')]
    [AutoNameTestCase('hckAlwaysChkSubj,$01')]
    [AutoNameTestCase('hckNoWildcard,$02')]
    [AutoNameTestCase('hckNoPartWildcard,$04')]
    [AutoNameTestCase('hckMultiLblWildcard,$08')]
    [AutoNameTestCase('hckSingleLblSubDomain,$10')]
    [AutoNameTestCase('hckAlwaysChkSubj;hckNoPartWildcard;hckSingleLblSubDomain,$15')]
    [AutoNameTestCase('hckAlwaysChkSubj;hckNoWildcard;hckNoPartWildcard;'+
      'hckMultiLblWildcard;hckSingleLblSubDomain,$1F')]
    procedure PositiveFlagsAsIntGet(AFlags: string; AIntVal: TIdC_UINT);

    [AutoNameTestCase('$20')]
    [AutoNameTestCase('$FFFFFFFF')]
    procedure NegativeFlagsAsIntSet(AIntVal: TIdC_UINT);

    [AutoNameTestCase(',$00')]
    [AutoNameTestCase('hckAlwaysChkSubj,$01')]
    [AutoNameTestCase('hckNoWildcard,$02')]
    [AutoNameTestCase('hckNoPartWildcard,$04')]
    [AutoNameTestCase('hckMultiLblWildcard,$08')]
    [AutoNameTestCase('hckSingleLblSubDomain,$10')]
    [AutoNameTestCase('hckAlwaysChkSubj;hckNoPartWildcard;hckSingleLblSubDomain,$15')]
    [AutoNameTestCase('hckAlwaysChkSubj;hckNoWildcard;hckNoPartWildcard;'+
      'hckMultiLblWildcard;hckSingleLblSubDomain,$1F')]
    procedure FlagsIsEqualTo(AFlags: string; AIntVal: TIdC_UINT);
  end;

  [TestFixture]
  TX509FvyParamPurposeFixture = class
  public type
    TPurpose = TaurusTLS_CustomX509VerifyParam.TPurpose;
  protected
    function GetFlagValue(AName: string): TPurpose;
    function GetFlagName(Value: TPurpose): string;
    function GetIntValue(Value: TPurpose): integer;

  public
    [AutoNameTestCase('$00,prpDefaultAny')]
    [AutoNameTestCase('$01,prpSslClient')]
    [AutoNameTestCase('$05,prpSMimeEncrypt')]
    [AutoNameTestCase('$08,prpOspHelper')]
    [AutoNameTestCase('$0A,prpCodeSign')]
    procedure PositivePurposeAsIntSet(AIntVal: TIdC_UINT; APurpose: TPurpose);

    [AutoNameTestCase('prpDefaultAny,$00')]
    [AutoNameTestCase('prpSslClient,$01')]
    [AutoNameTestCase('prpSMimeEncrypt,$05')]
    [AutoNameTestCase('prpOspHelper,$08')]
    [AutoNameTestCase('prpCodeSign,$0A')]
    procedure PositivePurposeAsIntGet(APurpose: TPurpose; AIntVal: TIdC_UINT);

    [AutoNameTestCase('$FF')]
    [AutoNameTestCase('$0B')]
    procedure NegativePurposeAsIntSet(AIntVal: TIdC_UINT);

    [AutoNameTestCase('prpDefaultAny,$00')]
    [AutoNameTestCase('prpNsSSLServer,$03')]
    [AutoNameTestCase('prpSMimeEncrypt,$05')]
    [AutoNameTestCase('prpAny,$07')]
    [AutoNameTestCase('prpCodeSign,$0A')]
    procedure PurposeIsEqualTo(APurpose: TPurpose; AIntVal: TIdC_UINT);
  end;

  [TestFixture]
  TX509FvyParamTrustFixture = class
  public type
    TTrust = TaurusTLS_CustomX509VerifyParam.TTrust;
  protected
    function GetFlagValue(AName: string): TTrust;
    function GetFlagName(Value: TTrust): string;
    function GetIntValue(Value: TTrust): TIdC_Int;

  public
    [AutoNameTestCase('$00,trDefault')]
    [AutoNameTestCase('$01,trCompat')]
    [AutoNameTestCase('$05,trObjectSign')]
    [AutoNameTestCase('$08,trTsa')]
    procedure PositivePurposeAsIntSet(AIntVal: TIdC_Int; ATrust: TTrust);

    [AutoNameTestCase('trDefault,$00')]
    [AutoNameTestCase('trCompat,$01')]
    [AutoNameTestCase('trObjectSign,$05')]
    [AutoNameTestCase('trTsa,$08')]
    procedure PositivePurposeAsIntGet(ATrust: TTrust; AIntVal: TIdC_Int);

    [AutoNameTestCase('$FF')]
    [AutoNameTestCase('$09')]
    procedure NegativePurposeAsIntSet(AIntVal: TIdC_Int);

    [AutoNameTestCase('trDefault,$00')]
    [AutoNameTestCase('trSslClient,$02')]
    [AutoNameTestCase('trObjectSign,$05')]
    [AutoNameTestCase('trOspSign,$06')]
    [AutoNameTestCase('trTsa,$08')]
    procedure TrustIsEqualTo(ATrust: TTrust; AIntVal: TIdC_Int);
  end;

implementation

uses
  System.Types;

{ TX509FvyParamVerifyFlagsFixture }

function TX509FvyParamVerifyFlagsFixture.GetFlagName(Value: TVerifyFlag): string;
begin
  Result:=cNames[Value];
end;

function TX509FvyParamVerifyFlagsFixture.GetFlagValue(AName: string): TVerifyFlag;
var
  i: TVerifyFlag;

begin
  for i:=Low(TVerifyFlag) to High(TVerifyFlag) do
    if CompareText(cNames[i], AName) = 0 then
      Exit(i);
  Assert.FailFmt('Invalid TVerifyFlag value: %s', [AName]);
end;

function TX509FvyParamVerifyFlagsFixture.GetIntValue(Value: TVerifyFlag): integer;
begin
  Result:=1 shl Ord(Value);
end;

function TX509FvyParamVerifyFlagsFixture.GetFlagsFromString(ANames: string): TVerifyFlags;
var
  lStrings: TStringDynArray;
  lStr: string;

begin
  Result:=[];
  lStrings:=SplitString(ANames, cFlagsDelim);
  for lStr in lStrings do
    Include(Result, GetFlagValue(lStr));
end;

procedure TX509FvyParamVerifyFlagsFixture.NegativeFlagAsIntSet(AIntVal: TIdC_ULONG);
begin
  Assert.WillRaise(
    procedure
    var
      lFlag: TVerifyFlag;

    begin
      lFlag.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamVerifyFlagsFixture.PositiveFlagAsIntSet(AIntVal: TIdC_ULONG;
  AFlag: string);
var
  lFlag: TVerifyFlag;
  lVal: TVerifyFlag;

begin
  lVal:=GetFlagValue(AFlag);
  lFlag.AsInt:=AIntVal;
  // Use "Assert.IsTrue" instead of "Assert.AreEqual<T> as TVerifyFlag
  // does not generate RTL information as enumeration does not start from Zero int.
  // As result, the Assert can't display enumeration name(s) in case of failure.
  Assert.IsTrue(lVal = lFlag, Format('Expected %s, got %s',[AFlag, GetFlagName(lFlag)]));
end;

procedure TX509FvyParamVerifyFlagsFixture.PositiveFlagAsIntGet(AFlag: string;
  AIntVal: TIdC_ULONG);
var
  lFlag: TVerifyFlag;
  lVal: integer;

begin
  lFlag:=GetFlagValue(AFlag);
  lVal:=lFlag.AsInt;
  Assert.AreEqual<integer>(AIntVal, lVal);
end;

procedure TX509FvyParamVerifyFlagsFixture.FlagIsEqualTo(AFlag: string;
  AIntVal: TIdC_ULONG);
var
  lFlag: TVerifyFlag;

begin
  lFlag:=GetFlagValue(AFlag);
  Assert.IsTrue(lFlag.IsEqualTo(AIntVal));
end;

procedure TX509FvyParamVerifyFlagsFixture.PositiveFlagsAsIntGet(AFlags: string;
  AIntVal: TIdC_ULONG);
var
  lFlags: TVerifyFlags;
  lVal: integer;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lVal:=lFlags.AsInt;
  Assert.AreEqual<TIdC_ULONG>(AIntVal, lVal);
end;

procedure TX509FvyParamVerifyFlagsFixture.PositiveFlagsAsIntSet(AIntVal: TIdC_ULONG;
  AFlags: string);
var
  lFlags: TVerifyFlags;
  lValues: TVerifyFlags;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lValues.AsInt:=AIntVal;
  Assert.AreEqual(lFlags, lValues);
end;

procedure TX509FvyParamVerifyFlagsFixture.PositiveFlagsSafeAsIntSet(
  AIntVal: TIdC_ULONG; AFlags: string);
var
  lFlags: TVerifyFlags;
  lValues: TVerifyFlags;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lValues.SafeAsInt:=AIntVal;
  Assert.AreEqual(lFlags, lValues);
end;

procedure TX509FvyParamVerifyFlagsFixture.NegativeFlagsAsIntSet(AIntVal: TIdC_ULONG);
begin
  Assert.WillRaise(
    procedure
    var
      lFlags: TVerifyFlags;

    begin
      lFlags.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamVerifyFlagsFixture.FlagsIsEqualTo(AFlags: string;
  AIntVal: TIdC_ULONG);
var
  lFlags: TVerifyFlags;
  lValues: TVerifyFlags;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lValues.AsInt:=AIntVal;
  Assert.IsTrue(lFlags.IsEqualTo(AIntVal));
end;

{ TX509FvyParamInteritanceFlagsFixture }

function TX509FvyParamInteritanceFlagsFixture.GetFlagName(
  Value: TInheritanceFlag): string;
begin
  Result:=TRttiEnumerationType.GetName<TInheritanceFlag>(Value);
end;

function TX509FvyParamInteritanceFlagsFixture.GetFlagValue(
  AName: string): TInheritanceFlag;
begin
  Result:=TRttiEnumerationType.GetValue<TInheritanceFlag>(AName);
end;

function TX509FvyParamInteritanceFlagsFixture.GetIntValue(
  Value: TInheritanceFlag): integer;
begin
  Result:=1 shl Ord(Value);
end;

function TX509FvyParamInteritanceFlagsFixture.GetFlagsFromString(
  ANames: string): TInheritanceFlags;
var
  lStrings: TStringDynArray;
  lStr: string;

begin
  Result:=[];
  lStrings:=SplitString(ANames, cFlagsDelim);
  for lStr in lStrings do
    Include(Result, GetFlagValue(lStr));
end;

procedure TX509FvyParamInteritanceFlagsFixture.PositiveFlagAsIntSet(
  AIntVal: TIdC_UINT32; AFlag: TInheritanceFlag);
var
  lFlag: TInheritanceFlag;

begin
  lFlag.AsInt:=AIntVal;
  Assert.AreEqual<TInheritanceFlag>(AFlag, lFlag);
end;

procedure TX509FvyParamInteritanceFlagsFixture.PositiveFlagAsIntGet(
  AFlag: string; AIntVal: TIdC_UINT32);
var
  lFlag: TInheritanceFlag;
  lVal: TIdC_UINT32;

begin
  lFlag:=GetFlagValue(AFlag);
  lVal:=lFlag.AsInt;
  Assert.AreEqual<TIdC_UINT32>(AIntVal, lVal);
end;

procedure TX509FvyParamInteritanceFlagsFixture.NegativeFlagAsIntSet(
  AIntVal: TIdC_UINT32; AFlag: TInheritanceFlag);
begin
  Assert.WillRaise(
    procedure
    var
      lFlag: TInheritanceFlag;

    begin
      lFlag.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamInteritanceFlagsFixture.FlagIsEqualTo(
  AFlag: TInheritanceFlag; AIntVal: TIdC_UINT32);
begin
  Assert.IsTrue(AFlag.IsEqualTo(AIntVal));
end;

procedure TX509FvyParamInteritanceFlagsFixture.PositiveFlagsAsIntSet(
  AIntVal: TIdC_UINT32; AFlags: string);
var
  lFlags: TInheritanceFlags;
  lValues: TInheritanceFlags;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lValues.AsInt:=AIntVal;
  Assert.AreEqual(lFlags, lValues);
end;

procedure TX509FvyParamInteritanceFlagsFixture.PositiveFlagsAsIntGet(
  AFlags: string; AIntVal: TIdC_UINT32);
var
  lFlags: TInheritanceFlags;
  lVal: integer;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lVal:=lFlags.AsInt;
  Assert.AreEqual<TIdC_UINT32>(AIntVal, lVal);
end;

procedure TX509FvyParamInteritanceFlagsFixture.NegativeFlagsAsIntSet(
  AIntVal: TIdC_UINT32);
begin
  Assert.WillRaise(
    procedure
    var
      lFlag: TInheritanceFlags;

    begin
      lFlag.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamInteritanceFlagsFixture.FlagsIsEqualTo(
  AFlags: string; AIntVal: TIdC_UINT32);
var
  lFlags: TInheritanceFlags;

begin
  lFlags:=GetFlagsFromString(AFlags);
  Assert.IsTrue(lFlags.IsEqualTo(AIntVal));
end;

{ TX509FvyParamVerifyHostCheckFlagsFixture }

function TX509FvyParamHostCheckFlagsFixture.GetFlagName(
  Value: THostCheckFlag): string;
begin
  Result:=TRttiEnumerationType.GetName<THostCheckFlag>(Value);
end;

function TX509FvyParamHostCheckFlagsFixture.GetFlagValue(
  AName: string): THostCheckFlag;
begin
  Result:=TRttiEnumerationType.GetValue<THostCheckFlag>(AName);
end;

function TX509FvyParamHostCheckFlagsFixture.GetIntValue(
  Value: THostCheckFlag): integer;
begin
  Result:= 1 shl Ord(Value);
end;

function TX509FvyParamHostCheckFlagsFixture.GetFlagsFromString(
  ANames: string): THostCheckFlags;
var
  lStrings: TStringDynArray;
  lStr: string;

begin
  Result:=[];
  lStrings:=SplitString(ANames, cFlagsDelim);
  for lStr in lStrings do
    Include(Result, GetFlagValue(lStr));
end;

procedure TX509FvyParamHostCheckFlagsFixture.PositiveFlagAsIntSet(
  AIntVal: TIdC_UINT; AFlag: THostCheckFlag);
var
  lFlag: THostCheckFlag;

begin
  lFlag.AsInt:=AIntVal;
  Assert.AreEqual<THostCheckFlag>(AFlag, lFlag);
end;

procedure TX509FvyParamHostCheckFlagsFixture.PositiveFlagAsIntGet(
  AFlag: string; AIntVal: TIdC_UINT);
var
  lFlag: THostCheckFlag;
  lVal: TIdC_UINT;

begin
  lFlag:=GetFlagValue(AFlag);
  lVal:=lFlag.AsInt;
  Assert.AreEqual<TIdC_UINT>(AIntVal, lVal);
end;

procedure TX509FvyParamHostCheckFlagsFixture.NegativeFlagAsIntSet(
  AIntVal: TIdC_UINT; AFlag: THostCheckFlag);
begin
  Assert.WillRaise(
    procedure
    var
      lFlag: THostCheckFlag;

    begin
      lFlag.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamHostCheckFlagsFixture.FlagIsEqualTo(
  AFlag: THostCheckFlag; AIntVal: TIdC_UINT32);
begin
  Assert.IsTrue(AFlag.IsEqualTo(AIntVal));
end;

procedure TX509FvyParamHostCheckFlagsFixture.PositiveFlagsAsIntSet(
  AIntVal: TIdC_UINT; AFlags: string);
var
  lFlags: THostCheckFlags;
  lValues: THostCheckFlags;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lValues.AsInt:=AIntVal;
  Assert.AreEqual(lFlags, lValues);
end;

procedure TX509FvyParamHostCheckFlagsFixture.PositiveFlagsAsIntGet(
  AFlags: string; AIntVal: TIdC_UINT);
var
  lFlags: THostCheckFlags;
  lVal: integer;

begin
  lFlags:=GetFlagsFromString(AFlags);
  lVal:=lFlags.AsInt;
  Assert.AreEqual<TIdC_UINT>(AIntVal, lVal);
end;

procedure TX509FvyParamHostCheckFlagsFixture.NegativeFlagsAsIntSet(
  AIntVal: TIdC_UINT);
begin
  Assert.WillRaise(
    procedure
    var
      lFlag: THostCheckFlags;

    begin
      lFlag.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamHostCheckFlagsFixture.FlagsIsEqualTo(
  AFlags: string; AIntVal: TIdC_UINT);
var
  lFlags: THostCheckFlags;

begin
  lFlags:=GetFlagsFromString(AFlags);
  Assert.IsTrue(lFlags.IsEqualTo(AIntVal));
end;

{ TX509FvyParamPurposeFixture }

function TX509FvyParamPurposeFixture.GetFlagName(Value: TPurpose): string;
begin
  Result:=TRttiEnumerationType.GetName<TPurpose>(Value);
end;

function TX509FvyParamPurposeFixture.GetFlagValue(
  AName: string): TPurpose;
begin
  Result:=TRttiEnumerationType.GetValue<TPurpose>(AName);
end;

function TX509FvyParamPurposeFixture.GetIntValue(
  Value: TPurpose): integer;
begin
  Result:=Ord(Value);
end;

procedure TX509FvyParamPurposeFixture.PositivePurposeAsIntSet(
  AIntVal: TIdC_UINT; APurpose: TPurpose);
var
  lPurpose: TPurpose;

begin
  lPurpose.AsInt:=AIntVal;
  Assert.AreEqual<TPurpose>(APurpose, lPurpose);
end;

procedure TX509FvyParamPurposeFixture.PositivePurposeAsIntGet(
  APurpose: TPurpose; AIntVal: TIdC_UINT);
var
  lVal: TIdC_UINT;

begin
  lVal:=APurpose.AsInt;
  Assert.AreEqual<TIdC_UINT>(AIntVal, lVal);
end;

procedure TX509FvyParamPurposeFixture.NegativePurposeAsIntSet(
  AIntVal: TIdC_UINT);
begin
  Assert.WillRaise(
    procedure
    var
      lPurpose: TPurpose;

    begin
      lPurpose.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamPurposeFixture.PurposeIsEqualTo(APurpose: TPurpose;
  AIntVal: TIdC_UINT);
begin
  Assert.IsTrue(APurpose.IsEqualTo(AIntVal));
end;

{ TX509FvyParamTrustFixture }

function TX509FvyParamTrustFixture.GetFlagName(Value: TTrust): string;
begin
  Result:=TRttiEnumerationType.GetName<TTrust>(Value);
end;

function TX509FvyParamTrustFixture.GetFlagValue(AName: string): TTrust;
begin
  Result:=TRttiEnumerationType.GetValue<TTrust>(AName);
end;

function TX509FvyParamTrustFixture.GetIntValue(Value: TTrust): TIdC_Int;
begin
  Result:=Ord(Value);
end;

procedure TX509FvyParamTrustFixture.PositivePurposeAsIntSet(
  AIntVal: TIdC_Int; ATrust: TTrust);
var
  lTrust: TTrust;

begin
  lTrust.AsInt:=AIntVal;
  Assert.AreEqual<TTrust>(ATrust, lTrust);
end;

procedure TX509FvyParamTrustFixture.PositivePurposeAsIntGet(
  ATrust: TTrust; AIntVal: TIdC_Int);
var
  lVal: TIdC_Int;

begin
  lVal:=ATrust.AsInt;
  Assert.AreEqual<TIdC_Int>(AIntVal, lVal);
end;

procedure TX509FvyParamTrustFixture.NegativePurposeAsIntSet(
  AIntVal: TIdC_Int);
begin
  Assert.WillRaise(
    procedure
    var
      lTrust: TTrust;

    begin
      lTrust.AsInt:=AIntVal;
    end,
    EInvalidCast
  );
end;

procedure TX509FvyParamTrustFixture.TrustIsEqualTo(ATrust: TTrust;
  AIntVal: TIdC_Int);
begin
  Assert.IsTrue(ATrust.IsEqualTo(AIntVal));
end;

initialization
  TDUnitX.RegisterTestFixture(TX509FvyParamVerifyFlagsFixture);
  TDUnitX.RegisterTestFixture(TX509FvyParamInteritanceFlagsFixture);
  TDUnitX.RegisterTestFixture(TX509FvyParamHostCheckFlagsFixture);
  TDUnitX.RegisterTestFixture(TX509FvyParamPurposeFixture);
  TDUnitX.RegisterTestFixture(TX509FvyParamTrustFixture);
end.
