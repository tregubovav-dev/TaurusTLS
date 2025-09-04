unit TaurusTLS.UT.SSLContainersHelpers;

interface

uses
  System.Classes, System.SysUtils, DUnitX.TestFramework, DUnitX.Types,
  DUnitX.InternalDataProvider, DUnitX.TestDataProvider,
  IdGlobal, IdCTypes,
  TaurusTLS_SSLContainersHelpers;

type
  THexStrTestTool = record
    class procedure CheckHex(AHexStr: AnsiString; ACharSize: cardinal);
      overload; static;
    class procedure CheckHex(AHexStr: string);
      overload; static;
    class function FromHex(AHexStr: string; ACodePage: cardinal): TBytes;
      overload; static;
    class procedure FromHex(AHexStr: string; ACodePage: cardinal;
      out AStr: AnsiString); overload; static;
    class procedure FromHex(AHexStr: string; out AStr: RawByteString);
      overload; static;
    class procedure FromHex(AHexStr: string; out AStr: UTF8String);
      overload; static;
    class procedure FromHex(AHexStr: string; out AStr: UnicodeString);
      overload; static;
    class procedure FromHex(AHexStr: string; out ABytes: TBytes);
      overload; static;
  end;

  TWipeTestTool = record
    class procedure CheckWiped(AData: pointer; ASize: NativeUInt); overload; static;
    class procedure CheckWiped(AData: TBytes); overload; static;
    class procedure CheckWiped(AData: RawByteString); overload; static;
    class procedure CheckWiped(AData: UnicodeString); overload; static;
  end;

  TBytesValidator = record
  public
    class procedure AreEqual(const ABytes: TBytes; const AStr: string;
      AWithTrailingNull: boolean); overload; static;
    class procedure AreEqual(const ABytes: TBytes; const AStr: RawByteString;
      AWithTrailingNull: boolean); overload; static;
    class procedure AreEqual(const ABytes: TBytes; const AStream: TStream;
      ACount: NativeUInt); overload; static;
  end;

//  Hex-encoded UTF16LE string constants for tests cases.
const
  cTestString = // 'Test string'
    '5400650073007400200073007400720069006E006700';
  cLatinChars = // Latin Supplement symbols for CP_1252
    'A100A200A300A400A500A600A700A800A900AA00AB00AC00'+
    'AD00AE00AF00B000B100B200B300B400B500B600B700B800'+
    'B900BA00BB00BC00BD00BE00BF00C000C100C200C300C400'+
    'C500C600C700C800C900CA00CB00CC00CD00CE00CF00D000'+
    'D100D200D300D400D500D600D700D800D900DA00DB00DC00'+
    'DD00DE00DF00E000E100E200E300E400E500E600E700E800'+
    'E900EA00EB00EC00ED00EE00EF00F000F100F200F300F400'+
    'F500F600F700F800F900FA00FB00FC00FD00FE00FF00';
  cCyrChars = // Cyrillyc symols for CP_1251
    '10041104120413041404150416041704180419041A041B04'+
    '1C041D041E041F0420042104220423042404250426042704'+
    '280429042A042B042C042D042E042F043004310432043304'+
    '3404350436043704380439043A043B043C043D043E043F04'+
    '40044104420443044404450446044704480449044A044B04'+
    '4C044D044E044F04';
  cGreekChars = // Greek symbols for CP_1253
    '90039103920393039403950396039703980399039A039B03'+
    '9C039D039E039F03A003A103A303A403A503A603A703A803'+
    'A903AA03AB03AC03AD03AE03AF03B003B103B203B303B403'+
    'B503B603B703B803B903BA03BB03BC03BD03BE03BF03C003'+
    'C103C203C303C403C503C603C703C803C903CA03CB03CC03'+
    'CD03CE03';

  cRBTestString = // 'Test string' in a single byte hex encoding
    '5465737420737472696E67';
  cRBLatinChars = // Latin Supplement for CP_1252 a single byte hex encoding
    'A1A2A3A4A5A6A7A8A9AAABACADAEAFB0B1B2B3B4B5B6B7B8'+
    'B9BABBBCBDBEBFC0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0'+
    'D1D2D3D4D5D6D7D8D9DADBDCDDDEDFE0E1E2E3E4E5E6E7E8'+
    'E9EAEBECEDEEEFF0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF';
  cRBCyrChars = // Cyrillyc symols for CP_1251 in a single-byte encoding
    'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D1D2D3D4D5D6D7'+
    'D8D9DADBDCDDDEDFE0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'+
    'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF';
  cRBGreekChars = // Greek symbols for CP_1253 in a single-byte encoding
    'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D1D3D4D5D6D7D8'+
    'D9DADBDCDDDEDFE0E1E2E3E4E5E6E7E8E9EAEBECEDEEEFF0'+
    'F1F2F3F4F5F6F7F8F9FAFBFCFDFE';

  cUTF8TestString = // 'Test string' in UTF8 encoding
    '54657374737472696E67';
  cUTF8LatinChars =
    'C2A1C2A2C2A3C2A4C2A5C2A6C2A7C2A8C2A9C2AAC2ABC2AC'+
    'C2ADC2AEC2AFC2B0C2B1C2B2C2B3C2B4C2B5C2B6C2B7C2B8'+
    'C2B9C2BAC2BBC2BCC2BDC2BEC2BFC380C381C382C383C384'+
    'C385C386C387C388C389C38AC38BC38CC38DC38EC38FC390'+
    'C391C392C393C394C395C396C397C398C399C39AC39BC39C'+
    'C39DC39EC39FC3A0C3A1C3A2C3A3C3A4C3A5C3A6C3A7C3A8'+
    'C3A9C3AAC3ABC3ACC3ADC3AEC3AFC3B0C3B1C3B2C3B3C3B4'+
    'C3B5C3B6C3B7C3B8C3B9C3BAC3BBC3BCC3BDC3BEC3BF';
  cUTF8CyrChars = // Cyrillyc symbols in UTF8 encoding
    'D090D091D092D093D094D095D096D097D098D099D09AD09B'+
    'D09CD09DD09ED09FD0A0D0A1D0A2D0A3D0A4D0A5D0A6D0A7'+
    'D0A8D0A9D0AAD0ABD0ACD0ADD0AED0AFD0B0D0B1D0B2D0B3'+
    'D0B4D0B5D0B6D0B7D0B8D0B9D0BAD0BBD0BCD0BDD0BED0BF'+
    'D180D181D182D183D184D185D186D187D188D189D18AD18B'+
    'D18CD18DD18ED18F';
  cUTF8GreekChars = // Greek symbols in UTF8 encoding
    'CE90CE91CE92CE93CE94CE95CE96CE97CE98CE99CE9ACE9B'+
    'CE9CCE9DCE9ECE9FCEA0CEA1CEA3CEA4CEA5CEA6CEA7CEA8'+
    'CEA9CEAACEABCEACCEADCEAECEAFCEB0CEB1CEB2CEB3CEB4'+
    'CEB5CEB6CEB7CEB8CEB9CEBACEBBCEBCCEBDCEBECEBFCF80'+
    'CF81CF82CF83CF84CF85CF86CF87CF88CF89CF8ACF8BCF8C'+
    'CF8DCF8E';

type
  [TestFixture]
  [Category('Wiper')]
  TWiperFixture = class
  public
    [TestCase('Empty', '')]
    [TestCase('Single_byte', 'e2')]
    [TestCase('''Test string''', cTestString)]
    [TestCase('Latin_Chars', cLatinChars)]
    [TestCase('Cyrillic_Chars', cCyrChars)]
    [TestCase('Greek_Chars', cGreekChars)]
    procedure WipeBytes(AHexStr: string);
    [TestCase('Empty', '')]
    [TestCase('Latin_Chars', cLatinChars)]
    procedure WipeAnsiStr(AHexStr: string);
    [TestCase('Empty', '')]
    [TestCase('''Test string''', cTestString)]
    [TestCase('Latin_Chars', cLatinChars)]
    [TestCase('Cyrillic_Chars', cCyrChars)]
    [TestCase('Greek_Chars', cGreekChars)]
    procedure WipeUTF8Str(AHexStr: string);
    [TestCase('Empty', '')]
    [TestCase('Single_byte', 'e2')]
    [TestCase('''Test string''', cTestString)]
    [TestCase('Latin_Chars', cLatinChars)]
    procedure WipeRawByteStr(AHexStr: string);
    [TestCase('Empty', '')]
    [TestCase('Single_char', '0020')]
    [TestCase('''Test string''', cTestString)]
    [TestCase('Latin_Chars', cLatinChars)]
    [TestCase('Cyrillic_Chars', cCyrChars)]
    [TestCase('Greek_Chars', cGreekChars)]
    procedure WipeUnicodeStr(AHexStr: string);
  end;

  [TestFixture]
  [Category('BytesHelpers')]
  TBytesHelpersFixture = class
  public
    [TestCase('Empty_WithoutTermNull',',False')]
    [TestCase('Empty_WithTermNull',',True')]
    [TestCase('English_WithoutTermNull', cTestString+',False')]
    [TestCase('English_WithTermNull', cTestString+',True')]
    [TestCase('Latin_WithoutTermNull', cLatinChars+',False')]
    [TestCase('Latin_WithTermNull', cLatinChars+',True')]
    [TestCase('Cyrillic_WithoutTermNull', cCyrChars+',False')]
    [TestCase('Cyrillic_WithTermNull', cCyrChars+',True')]
    [TestCase('Greek_WithoutTermNull', cGreekChars+',False')]
    [TestCase('Greek_WithTermNull', cGreekChars+',True')]
    procedure CreateUnicodeStr(AHexStr: string; AWithTrailingNull: boolean);
    [TestCase('CP_ACP_Empty_WithoutTermNull',',False,0')]
    [TestCase('CP_ACP_Empty_WithTermNull',',True,0')]
    [TestCase('CP_ACP_English_WithoutTermNull', cTestString+',False,0')]
    [TestCase('CP_ACP_English_WithTermNull', cTestString+',True,0')]
    [TestCase('CP_1252_Latin_WithoutTermNull', cLatinChars+',False,1252')]
    [TestCase('CP_1252_Latin_WithTermNull', cLatinChars+',True,1252')]
    [TestCase('CP_1251_Cyrillic_WithoutTermNull', cCyrChars+',False,1251')]
    [TestCase('CP_1251_Cyrillic_WithTermNull', cCyrChars+',True,1251')]
    [TestCase('CP_1253_Greek_WithoutTermNull', cGreekChars+',False,1253')]
    [TestCase('CP_1253_Greek_WithTermNull', cGreekChars+',True,1253')]
    procedure CreateAnsiStr(AHexStr: string; AWithTrailingNull: boolean;
      ACodePage: cardinal = CP_ACP);
    [TestCase('Empty_WithoutTermNull',',False')]
    [TestCase('Empty_WithTermNull',',True')]
    [TestCase('English_WithoutTermNull', cTestString+',False')]
    [TestCase('English_WithTermNull', cTestString+',True')]
    [TestCase('Latin_WithoutTermNull', cLatinChars+',False')]
    [TestCase('Latin_WithTermNull', cLatinChars+',True')]
    [TestCase('Cyrillic_WithoutTermNull', cCyrChars+',False')]
    [TestCase('Cyrillic_WithTermNull', cCyrChars+',True')]
    [TestCase('Greek_WithoutTermNull', cGreekChars+',False')]
    [TestCase('Greek_WithTermNull', cGreekChars+',True')]
    procedure CreateUTF8Str(AHexStr: string; AWithTrailingNull: boolean);
    [TestCase('Empty_WithoutTermNull',',False')]
    [TestCase('Empty_WithTermNull',',True')]
    [TestCase('English_WithoutTermNull', cTestString+',False')]
    [TestCase('English_WithTermNull', cTestString+',True')]
    [TestCase('Latin_WithoutTermNull', cLatinChars+',False')]
    [TestCase('Latin_WithTermNull', cLatinChars+',True')]
    [TestCase('Cyrillic_WithoutTermNull', cCyrChars+',False')]
    [TestCase('Cyrillic_WithTermNull', cCyrChars+',True')]
    [TestCase('Greek_WithoutTermNull', cGreekChars+',False')]
    [TestCase('Greek_WithTermNull', cGreekChars+',True')]
    procedure CreateAndWipeUnicodeStr(AHexStr: string; AWithTrailingNull: boolean);
    [TestCase('English_WithoutTermNull', cRBTestString+',False')]
    [TestCase('English_WithTermNull', cRBTestString+',True')]
    [TestCase('Lanin_WithoutTermNull', cRBLatinChars+',False')]
    [TestCase('Latin_WithTermNull', cRBLatinChars+',True')]
    [TestCase('Cyrillic_WithoutTermNull', cRBCyrChars+',False')]
    [TestCase('Cyrillic_WithTermNull', cRBCyrChars+',True')]
    [TestCase('Greek_WithoutTermNull', cRBGreekChars+',False')]
    [TestCase('Greek_WithTermNull', cRBGreekChars+',True')]
    procedure CreateAndWipeRBStr(AHexStr: string; AWithTrailingNull: boolean);
    [TestCase('CP_ACP_Empty_WithoutTermNull',',False,0')]
    [TestCase('CP_ACP_Empty_WithTermNull',',True,0')]
    [TestCase('CP_ACP_English_WithoutTermNull', cTestString+',False,0')]
    [TestCase('CP_ACP_English_WithTermNull', cTestString+',True,0')]
    [TestCase('CP_1252_Latin_WithoutTermNull', cLatinChars+',False,1252')]
    [TestCase('CP_1252_Latin_WithTermNull', cLatinChars+',True,1252')]
    [TestCase('CP_1251_Cyrillic_WithoutTermNull', cCyrChars+',False,1251')]
    [TestCase('CP_1251_Cyrillic_WithTermNull', cCyrChars+',True,1251')]
    [TestCase('CP_1253_Greek_WithoutTermNull', cGreekChars+',False,1253')]
    [TestCase('CP_1253_Greek_WithTermNull', cGreekChars+',True,1253')]
    procedure CreateAndWipeAnsiStr(AHexStr: string; AWithTrailingNull: boolean;
      ACodePage: cardinal = CP_ACP);
    [TestCase('Empty_WithoutTermNull',',False')]
    [TestCase('Empty_WithTermNull',',True')]
    [TestCase('English_WithoutTermNull', cTestString+',False')]
    [TestCase('English_WithTermNull', cTestString+',True')]
    [TestCase('Latin_WithoutTermNull', cLatinChars+',False')]
    [TestCase('Latin_WithTermNull', cLatinChars+',True')]
    [TestCase('Cyrillic_WithoutTermNull', cCyrChars+',False')]
    [TestCase('Cyrillic_WithTermNull', cCyrChars+',True')]
    [TestCase('Greek_WithoutTermNull', cGreekChars+',False')]
    [TestCase('Greek_WithTermNull', cGreekChars+',True')]
    procedure CreateAndWipeUTF8Str(AHexStr: string; AWithTrailingNull: boolean);
    [TestCase('Empty_WithoutTermNull',',False')]
    [TestCase('Empty_WithTermNull',',True')]
    [TestCase('English_WithoutTermNull', cTestString+',False')]
    [TestCase('English_WithTermNull', cTestString+',True')]
    [TestCase('Latin_WithoutTermNull', cLatinChars+',False')]
    [TestCase('Latin_WithTermNull', cLatinChars+',True')]
    [TestCase('Cyrillic_WithoutTermNull', cCyrChars+',False')]
    [TestCase('Cyrillic_WithTermNull', cCyrChars+',True')]
    [TestCase('Greek_WithoutTermNull', cGreekChars+',False')]
    [TestCase('Greek_WithTermNull', cGreekChars+',True')]
    procedure CreateAsUTF8UnicodeStr(AHexStr: string; AWithTrailingNull: boolean);
    [TestCase('CP_ACP_Empty_WithoutTermNull',',False,0')]
    [TestCase('CP_ACP_Empty_WithTermNull',',True,0')]
    [TestCase('CP_ACP_English_WithoutTermNull', cTestString+',False,0')]
    [TestCase('CP_ACP_English_WithTermNull', cTestString+',True,0')]
    [TestCase('CP_1252_Latin_WithoutTermNull', cLatinChars+',False,1252')]
    [TestCase('CP_1252_Latin_WithTermNull', cLatinChars+',True,1252')]
    [TestCase('CP_1251_Cyrillic_WithoutTermNull', cCyrChars+',False,1251')]
    [TestCase('CP_1251_Cyrillic_WithTermNull', cCyrChars+',True,1251')]
    [TestCase('CP_1253_Greek_WithoutTermNull', cGreekChars+',False,1253')]
    [TestCase('CP_1253_Greek_WithTermNull', cGreekChars+',True,1253')]
    procedure CreateAsUTF8AnsiStr(AHexStr: string; AWithTrailingNull: boolean;
      ACodePage: cardinal = CP_ACP);

    [TestCase('Empty_WithoutTermNull',',False')]
    [TestCase('Empty_WithTermNull',',True')]
    [TestCase('English_WithoutTermNull', cTestString+',False')]
    [TestCase('English_WithTermNull', cTestString+',True')]
    [TestCase('Latin_WithoutTermNull', cLatinChars+',False')]
    [TestCase('Latin_WithTermNull', cLatinChars+',True')]
    [TestCase('Cyrillic_WithoutTermNull', cCyrChars+',False')]
    [TestCase('Cyrillic_WithTermNull', cCyrChars+',True')]
    [TestCase('Greek_WithoutTermNull', cGreekChars+',False')]
    [TestCase('Greek_WithTermNull', cGreekChars+',True')]
    procedure CreateAsUTF8AndWipeUnicodeStr(AHexStr: string; AWithTrailingNull: boolean);
    [TestCase('CP_ACP_Empty_WithoutTermNull',',False,0')]
    [TestCase('CP_ACP_Empty_WithTermNull',',True,0')]
    [TestCase('CP_ACP_English_WithoutTermNull', cTestString+',False,0')]
    [TestCase('CP_ACP_English_WithTermNull', cTestString+',True,0')]
    [TestCase('CP_1252_Latin_WithoutTermNull', cLatinChars+',False,1252')]
    [TestCase('CP_1252_Latin_WithTermNull', cLatinChars+',True,1252')]
    [TestCase('CP_1251_Cyrillic_WithoutTermNull', cCyrChars+',False,1251')]
    [TestCase('CP_1251_Cyrillic_WithTermNull', cCyrChars+',True,1251')]
    [TestCase('CP_1253_Greek_WithoutTermNull', cGreekChars+',False,1253')]
    [TestCase('CP_1253_Greek_WithTermNull', cGreekChars+',True,1253')]
    procedure CreateAsUTF8AndWipeAnsiStr(AHexStr: string; AWithTrailingNull: boolean;
      ACodePage: cardinal = CP_ACP);
  end;

  TBytesHelpersStreamFixture = class

  end;


implementation

{ THexStrTestTool }

class procedure THexStrTestTool.CheckHex(AHexStr: string);
var
  lLen, i: NativeUInt;

begin
  lLen:=Length(AHexStr);
  Assert.IsTrue(lLen mod (SizeOf(WideChar)) = 0,
    Format('Length(AHexStr) must be divisible by %d.', [SizeOf(WideChar)]));
  for i:=1 to lLen do
    Assert.IsTrue(CharInSet(AHexStr[i], ['0'..'9','a'..'f','A'..'F']),
      'AHexStr[%d] is not valid hexdemical symbol.')
end;

class function THexStrTestTool.FromHex(AHexStr: string; ACodePage: cardinal): TBytes;
var
  lEnc, lUEnc: TEncoding;
  lBytes: TBytes;
  lBytesSize: NativeUInt;

begin
  lBytesSize:=Length(AHexStr);
  if lBytesSize = 0 then
    Exit;

  lBytesSize:=lBytesSize div 2;
  lUEnc:=TEncoding.Unicode;
  CheckHex(AHexStr);

  if ACodePage = $FFFF then
    lEnc:=nil
  else
    lEnc:=TEncoding.GetEncoding(ACodePage);
  try
    if Assigned(lEnc) then
    begin
      SetLength(lBytes, lBytesSize);
      HexToBin(PChar(AHexStr), 0, lBytes, 0, lBytesSize);
      Result:=TEncoding.Convert(lUEnc, lEnc, lBytes)
    end
    else
      Result:=BytesOf(AHexStr);
  finally
    lEnc.Free;
  end;
end;

class procedure THexStrTestTool.FromHex(AHexStr: string; ACodePage: cardinal;
  out AStr: AnsiString);
var
  lBytes: TBytes;
  lLen: NativeUInt;

begin
  lBytes:=FromHex(AHexStr, ACodePage);
  lLen:=Length(lBytes);
  if lLen > 0 then
  begin
    SetLength(AStr, lLen);
    SetCodePage(RawByteString(AStr), ACodePage, False);
    Move(PByte(lBytes)^, PAnsiChar(AStr)^, lLen);
  end
  else
    AStr:='';
end;

class procedure THexStrTestTool.FromHex(AHexStr: string;
  out AStr: RawByteString);
var
  lLen: NativeUInt;

begin
  CheckHex(AHexStr);
  lLen:=Length(AHexStr) div 2;
  if lLen = 0 then
    Exit;
  SetLength(AStr, lLen);
  SetLength(AStr, HexToBin(PChar(AHexStr), PAnsiChar(AStr), lLen));
end;

class procedure THexStrTestTool.FromHex(AHexStr: string;
  out AStr: UTF8String);
begin
  FromHex(AHexStr, CP_UTF8, AnsiString(AStr));
end;

class procedure THexStrTestTool.FromHex(AHexStr: string;
  out AStr: UnicodeString);
var
  lBytes: TBytes;
  lLen: NativeUInt;

begin
  lBytes:=FromHex(AHexStr, 1200 {//UTF16-LE});
  lLen:=Length(lBytes);
  if lLen > 0 then
  begin
    SetLength(AStr, lLen div SizeOf(WideChar));
    Move(PByte(lBytes)^, PChar(AStr)^, lLen);
  end
  else
    AStr:='';
end;

class procedure THexStrTestTool.FromHex(AHexStr: string; out ABytes: TBytes);
begin
  ABytes:=BytesOf(AHexStr);
end;

class procedure THexStrTestTool.CheckHex(AHexStr: AnsiString; ACharSize: cardinal);
var
  lLen, i: NativeUInt;

begin
  lLen:=Length(AHexStr);
  Assert.IsTrue(lLen mod (2*ACharSize) = 0,
    Format('Length(AHexStr) must be divisible by %d.', [2*ACharSize]));
  for i:=1 to lLen do
    Assert.IsTrue(AHexStr[i] in ['0'..'9','a'..'f','A'..'F'],
      'AHexStr[%d] is not valid hexdemical symbol.')
end;

{ TWipeTestTool }

class procedure TWipeTestTool.CheckWiped(AData: pointer; ASize: NativeUInt);
var
  lLongPtr: PNativeUInt;
  lBytePtr: PByte;

begin
  if (ASize = 0) or (not Assigned(AData)) then
    Exit;
  lLongPtr:=AData;
{$POINTERMATH ON}
  while (lLongPtr+1) < (PByte(AData)+ASize) do
    if lLongPtr^ <> 0 then
      Assert.Fail('Data is not wiped out.')
    else
      Inc(lLongPtr);
  lBytePtr:=PByte(lLongPtr);
  while lBytePtr < (PByte(AData)+ASize) do
    if lBytePtr^ <> 0 then
      Assert.Fail('Data is not wiped out.')
    else
      Inc(lBytePtr);
{$POINTERMATH OFF}
end;

class procedure TWipeTestTool.CheckWiped(AData: TBytes);
begin
  CheckWiped(PByte(AData), Length(AData)*SizeOf(Byte));
end;

class procedure TWipeTestTool.CheckWiped(AData: RawByteString);
begin
  CheckWiped(PAnsiChar(AData), Length(AData)*SizeOf(AnsiChar));
end;

class procedure TWipeTestTool.CheckWiped(AData: UnicodeString);
begin
  CheckWiped(PWideChar(AData), Length(AData)*SizeOf(WideChar));
end;

{ TWiperFixture }

procedure TWiperFixture.WipeBytes(AHexStr: string);
var
  lBytes: TBytes;
  lLen: NativeUInt;

begin
  THexStrTestTool.FromHex(AHexStr, lBytes);
  lLen:=Length(lBytes);
  TWiper.Wipe(lBytes);
  TWipeTestTool.CheckWiped(lBytes, lLen);
end;

procedure TWiperFixture.WipeAnsiStr(AHexStr: string);
var
  lStr: AnsiString;

begin
  THexStrTestTool.FromHex(AHexStr, CP_ACP, lStr);
  TWiper.Wipe(lStr);
  TWipeTestTool.CheckWiped(lStr);
end;

procedure TWiperFixture.WipeRawByteStr(AHexStr: string);
var
  lStr: RawByteString;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  TWiper.Wipe(lStr);
  TWipeTestTool.CheckWiped(lStr);
end;

procedure TWiperFixture.WipeUTF8Str(AHexStr: string);
var
  lStr: UTF8String;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  TWiper.Wipe(lStr);
  TWipeTestTool.CheckWiped(lStr);
end;

procedure TWiperFixture.WipeUnicodeStr(AHexStr: string);
var
  lStr: UnicodeString;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  TWiper.Wipe(lStr);
  TWipeTestTool.CheckWiped(lStr);
end;

{ TBytesTester }

class procedure TBytesValidator.AreEqual(const ABytes: TBytes;
  const AStr: string; AWithTrailingNull: boolean);
var
  lBLen, lSLen: NativeUInt;

begin
  lBLen:=Length(ABytes);
  lSLen:=Length(AStr);
  if AWithTrailingNull then
    Inc(lSLen);
  Assert.AreEqual<NativeUInt>(lBLen, lSLen*SizeOf(WideChar),
    'Length in bytes of ABytes and A and AStr are not equal.');
  Assert.AreEqualMemory(PByte(ABytes), PWideChar(AStr), lBLen);
end;

class procedure TBytesValidator.AreEqual(const ABytes: TBytes;
  const AStr: RawByteString; AWithTrailingNull: boolean);
var
  lBLen, lSLen: NativeUInt;

begin
  lBLen:=Length(ABytes);
  lSLen:=Length(AStr);
  if AWithTrailingNull then
    Inc(lSLen);
  Assert.AreEqual<NativeUInt>(lBLen, lSLen*SizeOf(AnsiChar),
    'Length in bytes of ABytes and A and AStr are not equal.');
  Assert.AreEqualMemory(PByte(ABytes), PAnsiChar(AStr), lBLen);
end;

class procedure TBytesValidator.AreEqual(const ABytes: TBytes;
  const AStream: TStream; ACount: NativeUInt);
var
  lBLen, lSLen: NativeUInt;
  lBytes: TBytes;

begin
  lBLen:=Length(ABytes);
  lSLen:=AStream.Size-AStream.Position;
  Assert.IsTrue(lSLen >= ACount,
    Format('Remaining AStream size (%d), less than ACount (%d)', [lSLen, ACount]));
  Assert.AreEqual(ACount, lBLen, 'ACount and Length(ABytes) are not equal.');
  if lBLen = 0 then
    Exit;
  SetLength(lBytes, lBLen);
  AStream.Read(lBytes, ACount);
  Assert.AreEqual(lBytes, ABytes, 'ABytes and content of AStream are not equal.');
end;

{ TBytesHelpersFixture }

procedure TBytesHelpersFixture.CreateUnicodeStr(AHexStr: string;
  AWithTrailingNull: boolean);
var
  lStr: string;
  lBytes: TBytes;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  lBytes:=TBytesFactory.Create(lStr, AWithTrailingNull);
  Assert.IsTrue(not Odd(Length(lBytes)), 'Array length must be even.');
  TBytesValidator.AreEqual(lBytes, lStr, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAnsiStr(AHexStr: string;
  AWithTrailingNull: boolean; ACodePage: cardinal);
var
  lStr: AnsiString;
  lBytes: TBytes;

begin
  THexStrTestTool.FromHex(AHexStr, ACodePage, lStr);
  lBytes:=TBytesFactory.Create(lStr, AWithTrailingNull);
  TBytesValidator.AreEqual(lBytes, RawByteString(lStr), AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateUTF8Str(AHexStr: string;
  AWithTrailingNull: boolean);
var
  lStr: UTF8String;
  lBytes: TBytes;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  lBytes:=TBytesFactory.Create(lStr, AWithTrailingNull);
  TBytesValidator.AreEqual(lBytes, RawByteString(lStr), AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAndWipeUnicodeStr(AHexStr: string;
  AWithTrailingNull: boolean);
var
  lBytes: TBytes;
  lStr, lCopyStr: string;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  THexStrTestTool.FromHex(AHexStr, lCopyStr);
  lBytes:=TBytesFactory.CreateAndWipe(lCopyStr, AWithTrailingNull);
  TWipeTestTool.CheckWiped(lCopyStr);
  Assert.IsTrue(not Odd(Length(lBytes)), 'Array length must be even.');
  TBytesValidator.AreEqual(lBytes, lStr, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAndWipeRBStr(AHexStr: string;
  AWithTrailingNull: boolean);
var
  lBytes: TBytes;
  lStr, lCopyStr: RawByteString;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  THexStrTestTool.FromHex(AHexStr, lCopyStr);
  lBytes:=TBytesFactory.CreateAndWipe(lCopyStr, AWithTrailingNull);
  TWipeTestTool.CheckWiped(lCopyStr);
  TBytesValidator.AreEqual(lBytes, lStr, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAndWipeAnsiStr(AHexStr: string;
  AWithTrailingNull: boolean; ACodePage: cardinal);
var
  lBytes: TBytes;
  lStr, lCopyStr: AnsiString;

begin
  THexStrTestTool.FromHex(AHexStr, ACodePage, lStr);
  THexStrTestTool.FromHex(AHexStr, ACodePage, lCopyStr);
  lBytes:=TBytesFactory.CreateAndWipe(lCopyStr, AWithTrailingNull);
  TWipeTestTool.CheckWiped(lCopyStr);
  TBytesValidator.AreEqual(lBytes, lStr, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAndWipeUTF8Str(AHexStr: string;
  AWithTrailingNull: boolean);
var
  lBytes: TBytes;
  lStr, lCopyStr: UTF8String;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  THexStrTestTool.FromHex(AHexStr, lCopyStr);
  lBytes:=TBytesFactory.CreateAndWipe(lCopyStr, AWithTrailingNull);
  TWipeTestTool.CheckWiped(lCopyStr);
  TBytesValidator.AreEqual(lBytes, lStr, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAsUTF8UnicodeStr(AHexStr: string;
  AWithTrailingNull: boolean);
var
  lBytes: TBytes;
  lStr: string;
  lUTF8Str: UTF8String;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  THexStrTestTool.FromHex(AHexStr, lUTF8Str);
  lBytes:=TBytesFactory.CreateAsUTF8(lStr, AWithTrailingNull);
  TBytesValidator.AreEqual(lBytes, lUTF8Str, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAsUTF8AnsiStr(AHexStr: string;
  AWithTrailingNull: boolean; ACodePage: cardinal = CP_ACP);
var
  lBytes: TBytes;
  lStr: AnsiString;
  lUTF8Str: UTF8String;

begin
  THexStrTestTool.FromHex(AHexStr, ACodePage, lStr);
  THexStrTestTool.FromHex(AHexStr, lUTF8Str);
  lBytes:=TBytesFactory.CreateAsUTF8(lStr, AWithTrailingNull);
  TBytesValidator.AreEqual(lBytes, lUTF8Str, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAsUTF8AndWipeUnicodeStr(AHexStr: string;
  AWithTrailingNull: boolean);
var
  lBytes: TBytes;
  lStr, lCopyStr: string;
  lUTF8Str: UTF8String;

begin
  THexStrTestTool.FromHex(AHexStr, lStr);
  THexStrTestTool.FromHex(AHexStr, lCopyStr);
  THexStrTestTool.FromHex(AHexStr, lUTF8Str);
  lBytes:=TBytesFactory.CreateAsUTF8AndWipe(lCopyStr, AWithTrailingNull);
  TWipeTestTool.CheckWiped(lCopyStr);
  TBytesValidator.AreEqual(lBytes, lUTF8Str, AWithTrailingNull);
end;

procedure TBytesHelpersFixture.CreateAsUTF8AndWipeAnsiStr(AHexStr: string;
  AWithTrailingNull: boolean; ACodePage: cardinal);
var
  lBytes: TBytes;
  lStr, lCopyStr: AnsiString;
  lUTF8Str: UTF8String;

begin
  THexStrTestTool.FromHex(AHexStr, ACodePage, lStr);
  THexStrTestTool.FromHex(AHexStr, ACodePage, lCopyStr);
  THexStrTestTool.FromHex(AHexStr, lUTF8Str);
  lBytes:=TBytesFactory.CreateAsUTF8AndWipe(lCopyStr, AWithTrailingNull);
  TWipeTestTool.CheckWiped(lCopyStr);
  TBytesValidator.AreEqual(lBytes, lUTF8Str, AWithTrailingNull);
end;

// To avoid false positive "memory leak" error we explicitly initialize
// TEncoding.Unicode.
procedure InitEncoding;
begin
  TEncoding.Unicode;
end;

initialization
  TDUnitX.RegisterTestFixture(TWiperFixture);
  TDUnitX.RegisterTestFixture(TBytesHelpersFixture);
  TDUnitX.RegisterTestFixture(TBytesHelpersStreamFixture);

  InitEncoding;

end.
