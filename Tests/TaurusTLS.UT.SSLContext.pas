unit TaurusTLS.UT.SSLContext;

interface

uses
  System.Classes, System.SysUtils, DUnitX.TestFramework, DUnitX.Types,
  DUnitX.InternalDataProvider, DUnitX.TestDataProvider,
  IdGlobal, IdCTypes,
  TaurusTLS.UT.TestClasses, TaurusTLSHeaders_types,
  TaurusTLS_SSLContext;

type
  [TestFixture]
  [Category('Ciphers')]
  TCipherFixture = class(TOsslBaseFixture)
  private
    FCipher: TTaurusTLS_Cipher;
    FCipherName: string;
  protected
    function GetCipher(ACipherName: string): TTaurusTLS_Cipher;
    procedure DoWithOSSLCipher(AOSSLCipherProc: TProc<PEVP_CIPHER>);
    function CheckRandomFactor(ABytes: TBytes): NativeInt;

    procedure CheckCipherKeyLen(const ACipherName: string; ALen: TIdC_UINT);
    procedure CheckCipherIVLen(const ACipherName: string; ALen: TIdC_UINT);
    procedure CheckCipherBlockSize(const ACipherName: string; ALen: TIdC_UINT);

  public
    property Cipher[ACipherName: string]: TTaurusTLS_Cipher read GetCipher;

    [Teardown]
    procedure Teardown;
    [AutoNameTestCase('DES3')]
    [AutoNameTestCase('ChaCha20')]
    [AutoNameTestCase('AES-128-CBC-CTS')]
    [AutoNameTestCase('ARIA-256-GCM')]
    [AutoNameTestCase('CAMELLIA-256-CFB')]
    procedure GetCipherByNameUnicodePositive(const ACipherName: string);
    [AutoNameTestCase('DES3')]
    [AutoNameTestCase('ChaCha20')]
    [AutoNameTestCase('AES-128-CBC-CTS')]
    [AutoNameTestCase('ARIA-256-GCM')]
    [AutoNameTestCase('CAMELLIA-256-CFB')]
    procedure GetCipherByNameAnsiPositive(const ACipherName: RawByteString);
    [TestCase('aes-wrong-name', 'aes-wrong-name')]
    procedure GetCipherByNameUnicodeNegative(const ACipherName: string);
    [TestCase('aes-wrong-name', 'aes-wrong-name')]
    procedure GetCipherByNameAnsiNegative(const ACipherName: RawByteString);
    [AutoNameTestCase('DES3')]
    [AutoNameTestCase('ChaCha20')]
    [AutoNameTestCase('AES-128-CBC-CTS')]
    [AutoNameTestCase('ARIA-256-GCM')]
    [AutoNameTestCase('CAMELLIA-256-CFB')]
    procedure NewKey(const ACipherName: string);
    [AutoNameTestCase('DES3')]
    [AutoNameTestCase('ChaCha20')]
    [AutoNameTestCase('AES-128-CBC-CTS')]
    [AutoNameTestCase('ARIA-256-GCM')]
    [AutoNameTestCase('CAMELLIA-256-CFB')]
    procedure NewIV(const ACipherName: string);
    [AutoNameTestCase('DES3')]
    [AutoNameTestCase('ChaCha20')]
    [AutoNameTestCase('AES-128-CBC-CTS')]
    [AutoNameTestCase('ARIA-256-GCM')]
    [AutoNameTestCase('CAMELLIA-256-CFB')]
    procedure KeyLen(const ACipherName: string);
    [AutoNameTestCase('DES3')]
    [AutoNameTestCase('ChaCha20')]
    [AutoNameTestCase('AES-128-CBC-CTS')]
    [AutoNameTestCase('ARIA-256-GCM')]
    [AutoNameTestCase('CAMELLIA-256-CFB')]
    procedure IVLen(const ACipherName: string);
    [AutoNameTestCase('DES3')]
    [AutoNameTestCase('ChaCha20')]
    [AutoNameTestCase('AES-128-CBC-CTS')]
    [AutoNameTestCase('ARIA-256-GCM')]
    [AutoNameTestCase('CAMELLIA-256-CFB')]
    procedure BlockSize(const ACipherName: string);
  end;

//  TAESKeySize = TTaurusTLS_AESKeySize;
//  TEncodeMode = TTaurusTLS_EncodeMode;

  [TestFixture]
  [Category('Encryptor')]
  TSimpleAESEncryptorFixture = class(TOsslBaseFixture)
  private var
    FKeySizeNames: array[TTaurusTLS_AESKeySize] of string;
    FEncoderModeNames: array[TTaurusTLS_EncodeMode] of string;

    function CompareBytes(const A, B: TBytes): boolean;
    procedure EncryptDecrypt(AEncryptor: TTaurusTLS_SimpleAESEncryptor;
      ADataSize: NativeUInt); overload;
    function GetKeySizeByName(AKeySizeName: string): TTaurusTLS_AESKeySize;
    function GetEncodingModeByName(AEncoderModeName: string): TTaurusTLS_EncodeMode;
  protected
    property KeySizeByName[AKeySizeName: string]: TTaurusTLS_AESKeySize
      read GetKeySizeByName;
    property EncodingModeByName[AEncoderModeName: string]: TTaurusTLS_EncodeMode
      read GetEncodingModeByName;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [AutoNameTestCase('CBC,1')]
    [AutoNameTestCase('CBC,1024')]
    [AutoNameTestCase('CBC,383')]
    [AutoNameTestCase('CFB,1')]
    [AutoNameTestCase('CFB,1024')]
    [AutoNameTestCase('CFB,383')]
    [AutoNameTestCase('CFB,1')]
    [AutoNameTestCase('CFB,1024')]
    [AutoNameTestCase('CFB,383')]
    [AutoNameTestCase('CTR,1')]
    [AutoNameTestCase('CTR,1024')]
    [AutoNameTestCase('CTR,383')]
    procedure TestEncryptor(AEncoderMode: string; ADataSize: NativeUInt);
  end;

  TCustomBytesFixture = class(TOsslBaseFixture)
    FData: TBytes;
    function GetDataLen: NativeUInt;
  protected
    procedure InitData(ASize: TIdC_SizeT); overload;
    procedure InitData(AData: TBytes); overload;
    function CopyData: TBytes;
    class function HexToBytes(const AHexStr: string): TBytes;
    class function Base64ToBytes(const AB64: string): TBytes;
    class function GetBioMemPtr(ABio: ITaurusTLS_Bio; var AMemPtr: pointer): TIdC_INT;
    class procedure TestBytes(AExpected, AData: TBytes); overload;
    class procedure TestData(AExpected: TBytes; AData: pointer; ADataLen: NativeUInt); overload;
    procedure TestBytes(AData: TBytes); overload;
    procedure TestData(AData: pointer; ADataLen: NativeUInt); overload;
    procedure TestBytesIntf(ABytes: ITaurusTLS_Bytes);
    procedure TestBioIntf(ABio: ITaurusTLS_Bio);

    property Data: TBytes read FData;
    property DataLen: NativeUInt read GetDataLen;
  public
    [Teardown]
    procedure TearDown;
  end;

  [TestFixture]
  [Category('Bytes')]
  TBytesFixture = class(TCustomBytesFixture)
  public
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure BytesSame(AHexStr: string); overload;
    [AutoNameTestCase('0')]
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure BytesSame(ASize: NativeUInt); overload;
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure BytesCopy(AHexStr: string); overload;
    [AutoNameTestCase('0')]
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure BytesCopy(ASize: NativeUInt); overload;
    [Test]
    procedure BioNull(ASize: NativeUInt); overload;
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure BioSame(AHexStr: string); overload;
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure BioSame(ASize: NativeUInt); overload;
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure BioCopy(AHexStr: string); overload;
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure BioCopy(ASize: NativeUInt); overload;
    [AutoNameTestCase('383, 1')]
    [AutoNameTestCase('383, 15')]
    procedure BioRepeat(ASize, ACount: NativeUInt);
    [AutoNameTestCase('383, 1')]
    [AutoNameTestCase('383, 15')]
    procedure BioRepeatOneLock(ASize, ACount: NativeUInt);
  end;

  [TestFixture]
  [Category('WipingBytes')]
  TWipingBytesFixture = class(TCustomBytesFixture)
  protected
    class procedure IsEmpty(ABytes: TBytes); overload;
    class procedure IsEmpty(ABio: PBIO; AExpectedLen: NativeUInt); overload;
    class procedure IsEmpty(AData: pointer; ASize: NativeUInt); overload;
    procedure TestWipeBytes;
    procedure TestWipeBio;
  public
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure WipeBytes(AHexStr: string); overload;
    [AutoNameTestCase('0')]
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure WipeBytes(ASize: NativeUInt); overload;
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure WipeBio(AHexStr: string); overload;
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure WipeBio(ASize: NativeUInt); overload;
  end;

  [TestFixture]
  [Category('EncryptedBytes')]
  TEnryptedBytesFixture = class(TCustomBytesFixture)
  protected
    procedure IsEncrypted(const ABytes: TBytes); overload;
    procedure IsEncrypted(AData: pointer; ADataLen: NativeUInt); overload;
    procedure IsUnEncrypted(ABio: ITaurusTLS_Bio); overload;
    class function NewBytes(ABytes: TBytes): ITaurusTLS_Bytes; overload;
    class function NewBytes(AHexStr: string): ITaurusTLS_Bytes; overload;
  public
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure Bytes(AHexStr: string); overload;
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure Bytes(ASize: NativeUInt); overload;
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure Bio(AHexStr: string); overload;
    [AutoNameTestCase('1024')]
    [AutoNameTestCase('177')]
    [AutoNameTestCase('383')]
    procedure Bio(ASize: NativeUInt); overload;
    [AutoNameTestCase('383, 1')]
    [AutoNameTestCase('383, 15')]
    procedure BioRepeat(ASize, ACount: NativeUInt);
    [AutoNameTestCase('383, 1')]
    [AutoNameTestCase('383, 15')]
    procedure BioRepeatOneLock(ASize, ACount: NativeUInt);
  end;

  [TestFixture]
  [Category('SimpleAESFactory')]
  TSimpleAESFactoryFixture = class(TOsslBaseFixture)
  public
    [Test]
    procedure KeySizeNameAll;
    [Test]
    procedure EncodeModeNameAll;
    [AutoNameTestCase('128,CBC')]
    [AutoNameTestCase('192,CFB')]
    [AutoNameTestCase('256,OFB')]
    procedure NewEncryptor(AKeySizeName, AEncodeMode: string);
    [IgnoreMemoryLeaks] // The singleton intsantiates first time.
    [AutoNameTestCase('128,OFB')]
    [AutoNameTestCase('192,CBC')]
    [AutoNameTestCase('256,CFB')]
    procedure SetDefaultCipher(AKeySizeName, AEncodeMode: string);
    [AutoNameTestCase('128,OFB')]
    [AutoNameTestCase('192,CBC')]
    [AutoNameTestCase('256,CFB')]
    procedure NewEncyptorDefault(AKeySizeName, AEncodeMode: string);
  end;

  [TestFixture]
  [Category('BytesHelpers')]
  TBytesHelperFixture = class(TCustomBytesFixture)
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure LoadFromBytes(AHexStr: string);
    [AutoNameTestCase('0123456789ABCDEF')]
    procedure LoadFromPAnsiStr(AData: RawByteString);
    [AutoNameTestCase('0123456789ABCDEF')]
    procedure LoadFromString(AData: string);
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure LoadFromMemStream(AHexStr: string);
    [AutoNameTestCase('e2')]
    [AutoNameTestCase('026ca6b4d0761ebfcd933a3f7b379e')]
    [AutoNameTestCase('941833f8ffd98ea2861d8c9f718d3f4d'+
      '7ca37f0214e733d1bd1426bfa03fa2c0')]
    procedure LoadFromByteStream(AHexStr: string);
  end;

  [TestFixture]
  [Category('EncyptedBytesHelpers')]
  TEncyptedBytesHelperFixture = class(TCustomBytesFixture)

  end;


implementation

uses
  System.RTTI, System.NetEncoding, System.TypInfo, TaurusTLSHeaders_bio,
  TaurusTLSHeaders_evp, TaurusTLSHeaders_evperr, TaurusTLS_Random;

type
  TSimpleAESEnums = class
  protected const
    // DO NOT LOCALIZE
    cKeySizes: array[TTaurusTLS_AESKeySize] of string = ('128','192','256');
    cIntKeySizes: array[TTaurusTLS_AESKeySize] of cardinal = (128, 192, 256);
    cEncoderModes: array[TTaurusTLS_EncodeMode] of string =
      ('CBC','CFB','OFB','CTR');
  protected
    class function GetKeySizeNameToEnum(AName: string): TTaurusTLS_AESKeySize;
      static;
    class function GetEncodeModeNameToEnum(AName: string): TTaurusTLS_EncodeMode;
      static;
    class function GetEncodeModeToName(AValue: TTaurusTLS_EncodeMode): string;
      static;
    class function GetKeySizeEnumToName(AValue: TTaurusTLS_AESKeySize): string;
      static;
    class function GetKeySizeNameToInt(AValue: TTaurusTLS_AESKeySize): cardinal;
      static;
  public
    class property KeySize[AName: string]: TTaurusTLS_AESKeySize
      read GetKeySizeNameToEnum;
    class property KeySizeName[AValue: TTaurusTLS_AESKeySize]: string
      read GetKeySizeEnumToName;
    class property EncodeMode[AName: string]: TTaurusTLS_EncodeMode
      read GetEncodeModeNameToEnum;
    class property EncodeModeName[AValue: TTaurusTLS_EncodeMode]: string
      read GetEncodeModeToName;
    class property KeySizeInt[AValue: TTaurusTLS_AESKeySize]: cardinal
      read GetKeySizeNameToInt;
  end;

class function TSimpleAESEnums.GetEncodeModeNameToEnum(
  AName: string): TTaurusTLS_EncodeMode;
begin
  for Result:=Low(TTaurusTLS_EncodeMode) to High(TTaurusTLS_EncodeMode) do
    if AName.CompareTo(cEncoderModes[Result]) = 0 then
      Exit;
  Assert.FailFmt('Unable to convert string value ''%s'' to the value '+
    'of ''TTaurusTLS_EncodeMode'' type.', [AName]);
end;

class function TSimpleAESEnums.GetEncodeModeToName(
  AValue: TTaurusTLS_EncodeMode): string;
begin
  Result:=cEncoderModes[AValue];
end;

class function TSimpleAESEnums.GetKeySizeEnumToName(
  AValue: TTaurusTLS_AESKeySize): string;
begin
  Result:=cKeySizes[AValue];
end;

class function TSimpleAESEnums.GetKeySizeNameToEnum(
  AName: string): TTaurusTLS_AESKeySize;
begin
  for Result:=Low(TTaurusTLS_AESKeySize) to High(TTaurusTLS_AESKeySize) do
    if AName.CompareTo(cKeySizes[Result]) = 0 then
      Exit;
  Assert.FailFmt('Unable to convert string value ''%s'' to the value '+
    'of ''TTaurusTLS_EncodeMode'' type.', [AName]);
end;

class function TSimpleAESEnums.GetKeySizeNameToInt(
  AValue: TTaurusTLS_AESKeySize): cardinal;
begin
  Result:=cIntKeySizes[AValue];
end;

{ TCipherFixture }

function TCipherFixture.GetCipher(ACipherName: string): TTaurusTLS_Cipher;
begin
  if CompareText(FCipherName, ACipherName) <> 0 then
  begin
    FreeAndNil(FCipher);
    FCipherName:=ACipherName;
  end;
  if not Assigned(FCipher) then
    FCipher:=TTaurusTLS_Cipher.Create(FCipherName);
  Assert.IsNotNull(FCipher,
    Format('Unable to create TTaurusTLS_Cipher instance '+
    'with the cipher name ''%s''.', [ACipherName]));
  Result:=FCipher;
end;

function TCipherFixture.CheckRandomFactor(ABytes: TBytes): NativeInt;
var
  i, lChunkStart, lChunkEnd: NativeInt;
  lBits: set of byte;
  lVal, lUniqueCount: byte;

begin
  Result:=Length(ABytes);
  if Result = 0 then
    Exit;

  lChunkStart:=0;
  repeat
    lChunkEnd:=lChunkStart+High(Byte)+1;
    if lChunkEnd >= Result then
      lChunkEnd:=Result-1;
    lBits:=[];
    for i:=lChunkStart to lChunkEnd do
    begin
      lVal:=ABytes[i];
      if not (lVal in lBits) then
      begin
//        lUniqueCount:=
      end;
    end;

    Inc(lChunkStart);
  until lChunkEnd >= Result;


end;

procedure TCipherFixture.DoWithOSSLCipher(AOSSLCipherProc: TProc<PEVP_CIPHER>);
var
  lOSSLCipher: PEVP_CIPHER;
  lCipherName: RawByteString;

begin
  if not Assigned(AOSSLCipherProc) then
    Assert.Fail(ClassName+'.DoWithOSSLCipher: AOSSLCipherProc should not be ''nil''.');
  lOSSLCipher:=nil;
  lCipherName:=FCipherName;
  try
    lOSSLCipher:=EVP_Cipher_fetch(nil, PIdAnsiChar(lCipherName), nil);
    if not Assigned(lOSSLCipher) then
      Assert.Fail(Format('Unable to initialize Cipher the name ''%s''.',
        [FCipherName]));
    AOSSLCipherProc(lOSSLCipher);
  finally
    if Assigned(lOSSLCipher) then
      EVP_CIPHER_free(lOSSLCipher);
  end;
end;

procedure TCipherFixture.CheckCipherBlockSize(const ACipherName: string;
  ALen: TIdC_UINT);
var
  lOSSLCipherBlockSize: TIdC_UINT;

begin
  DoWithOSSLCipher(
    procedure(AOSSLCipher: PEVP_CIPHER)
    begin
      lOSSLCipherBlockSize:=EVP_CIPHER_get_block_size(AOSSLCipher);
      Assert.AreNotEqual<TIdC_UINT>(0, lOSSLCipherBlockSize,
        'Cipher Block Size is not initialized.');
    end
  );

  Assert.AreEqual<TIdC_UINT>(lOSSLCipherBlockSize, ALen,
    'Invalid Cipher Block Length.');
end;

procedure TCipherFixture.CheckCipherKeyLen(const ACipherName: string;
  ALen: TIdC_UINT);
var
  lOSSLCipherLen: TIdC_UINT;

begin
  DoWithOSSLCipher(
    procedure(AOSSLCipher: PEVP_CIPHER)
    begin
      lOSSLCipherLen:=EVP_CIPHER_get_key_length(AOSSLCipher);
      if lOSSLCipherLen = 0 then
        lOSSLCipherLen:=EVP_MAX_KEY_LENGTH;
    end
  );

  Assert.AreEqual<TIdC_UINT>(lOSSLCipherLen, ALen,
    'Invalid Cipher Key Length.');
end;

procedure TCipherFixture.CheckCipherIVLen(const ACipherName: string; ALen: TIdC_UINT);
var
  lOSSLCipherIVLen: TIdC_UINT;

begin
  DoWithOSSLCipher(
    procedure(AOSSLCipher: PEVP_CIPHER)
    begin
      lOSSLCipherIVLen:=EVP_CIPHER_get_iv_length(AOSSLCipher);
      if lOSSLCipherIVLen = 0 then
        lOSSLCipherIVLen:=EVP_MAX_KEY_LENGTH;
    end
  );

  Assert.AreEqual<TIdC_UINT>(lOSSLCipherIVLen, ALen,
    'Invalid Cipher IV Length.');
end;

procedure TCipherFixture.Teardown;
begin
  FreeAndNil(FCipher);
end;

procedure TCipherFixture.GetCipherByNameAnsiNegative(
  const ACipherName: RawByteString);
begin
  Assert.IsNull(TTaurusTLS_Cipher.GetCipherByName(PIdAnsiChar(ACipherName)),
    'TTaurusTLS_Cipher.GetCipherByName should return '+
    'empty Cipher method implementation for this Cipher name.')
end;

procedure TCipherFixture.GetCipherByNameAnsiPositive(
  const ACipherName: RawByteString);
begin
  Assert.IsNotNull(TTaurusTLS_Cipher.GetCipherByName(PIdAnsiChar(ACipherName)),
    'TTaurusTLS_Cipher.GetCipherByName returns empty Cipher method implementation.')
end;

procedure TCipherFixture.GetCipherByNameUnicodeNegative(const ACipherName: string);
begin
  Assert.IsNull(TTaurusTLS_Cipher.GetCipherByName(ACipherName),
    'TTaurusTLS_Cipher.GetCipherByName should return '+
    'empty Cipher method implementation for this Cipher name.')
end;

procedure TCipherFixture.GetCipherByNameUnicodePositive(const ACipherName: string);
begin
  Assert.IsNotNull(TTaurusTLS_Cipher.GetCipherByName(ACipherName),
    'TTaurusTLS_Cipher.GetCipherByName returns empty Cipher method implementation.')
end;

procedure TCipherFixture.NewKey(const ACipherName: string);
begin
  CheckCipherKeyLen(ACipherName, Length(Cipher[ACipherName].NewKey));
end;

procedure TCipherFixture.NewIV(const ACipherName: string);
begin
  CheckCipherIVLen(ACipherName, Length(Cipher[ACipherName].NewIV));
end;

procedure TCipherFixture.KeyLen(const ACipherName: string);
begin
  CheckCipherKeyLen(ACipherName, Cipher[ACipherName].KeyLen);
end;

procedure TCipherFixture.IVLen(const ACipherName: string);
begin
  CheckCipherIVLen(ACipherName, Cipher[ACipherName].IVLen);
end;

procedure TCipherFixture.BlockSize(const ACipherName: string);
begin
  CheckCipherBlockSize(ACipherName, Cipher[ACipherName].BlockSize);
end;

{ TAESEncryptDecryptFixture }

procedure TSimpleAESEncryptorFixture.Setup;
var
  lKeySize: TTaurusTLS_AESKeySize;
  lEncoderMode: TTaurusTLS_EncodeMode;

begin
  for lKeySize:=Low(TTaurusTLS_AESKeySize) to High(TTaurusTLS_AESKeySize) do
    FKeySizeNames[lKeySize]:=TTaurusTLS_SimpleAESFactory.KeySizeName[lKeySize];
  for lEncoderMode:=Low(TTaurusTLS_EncodeMode) to High(TTaurusTLS_EncodeMode) do
    FEncoderModeNames[lEncoderMode]:=
      TTaurusTLS_SimpleAESFactory.EncodeModeName[lEncoderMode];
end;

procedure TSimpleAESEncryptorFixture.Teardown;
var
  lKeySize: TTaurusTLS_AESKeySize;
  lEncoderMode: TTaurusTLS_EncodeMode;

begin
  for lKeySize:=Low(TTaurusTLS_AESKeySize) to High(TTaurusTLS_AESKeySize) do
    FKeySizeNames[lKeySize]:='';
  for lEncoderMode:=Low(TTaurusTLS_EncodeMode) to High(TTaurusTLS_EncodeMode) do
    FEncoderModeNames[lEncoderMode]:='';
end;

function TSimpleAESEncryptorFixture.CompareBytes(const A, B: TBytes): boolean;
var
  lLenA, lLenb, lMinLen: NativeUInt;

begin
  lLenA:=Length(A);
  lLenB:=Length(B);
  if lLenA < lLenB then
    lMinLen:=lLenA
  else
    lMinLen:=lLenB;
  Result:=CompareMem(@A[0], @B[0], lMinLen);
end;

procedure TSimpleAESEncryptorFixture.EncryptDecrypt(
  AEncryptor: TTaurusTLS_SimpleAESEncryptor; ADataSize: NativeUInt);
var
  lPlain, lEnc, lDec: TBytes;

begin
  Assert.IsNotNull(AEncryptor, 'AEncryptor must not be ''nil''.');
  Assert.AreNotEqual<TIdC_UINT>(0, ADataSize, 'ADataSize is Zero.');
  lPlain:=TTaurusTLS_Random.PublicRandom.Random(ADataSize);

  AEncryptor.Encrypt(lPlain, lEnc);
  Assert.AreNotEqual<NativeUInt>(0, Length(lEnc), 'Encrypted size is Zero.');
  Assert.IsFalse(CompareBytes(lPlain, lEnc),
    'Content of Plan and Encrypted data is partially or fully equal.');

  AEncryptor.Decrypt(lEnc, lDec);
  Assert.AreNotEqual<NativeUInt>(0, Length(lEnc),  'Decrypted size is Zero.');
  Assert.IsTrue(CompareBytes(lPlain, lDec),
    'Content of Plan and Encrypted data is NOT partially or fully equal.');
  Assert.AreEqual(Length(lPlain), Length(lDec),
    'Length of Plain and Decrypted data are not equal.')
end;

function TSimpleAESEncryptorFixture.GetKeySizeByName(
  AKeySizeName: string): TTaurusTLS_AESKeySize;
begin
  for Result:=Low(Result) to High(Result) do
    if CompareStr(FKeySizeNames[Result], AKeySizeName) = 0 then
      Exit;
  Assert.Fail(Format('Unknown TTaurusTLS_AESKeySize name ''%s''.',
    [AKeySizeName]));
end;

function TSimpleAESEncryptorFixture.GetEncodingModeByName(
  AEncoderModeName: string): TTaurusTLS_EncodeMode;
begin
  for Result:=Low(Result) to High(Result) do
    if CompareStr(FEncoderModeNames[Result], AEncoderModeName) = 0 then
      Exit;
  Assert.Fail(Format('Unknown TTaurusTLS_AESKeySize name ''%s''.',
    [AEncoderModeName]));
end;

procedure TSimpleAESEncryptorFixture.TestEncryptor(AEncoderMode: string;
  ADataSize: NativeUInt);
var
  lEncryptor: TTaurusTLS_SimpleAESEncryptor;
  lKeySize: TTaurusTLS_AESKeySize;

begin
  for lKeySize:=Low(TTaurusTLS_AESKeySize) to High(TTaurusTLS_AESKeySize) do
  begin
    lEncryptor:=nil;
    try
      lEncryptor:=TTaurusTLS_SimpleAESEncryptor.Create(lKeySize,
        EncodingModeByName[AEncoderMode]);
      EncryptDecrypt(lEncryptor, ADataSize);
    finally
      lEncryptor.Free;
    end;
  end;
end;

{ TCustomBytesFixture }

procedure TCustomBytesFixture.TearDown;
begin
  FData:=nil;
end;

class function TCustomBytesFixture.Base64ToBytes(const AB64: string): TBytes;
begin
  Assert.AreNotEqual(0, Length(AB64),
    'Parameter ''AB64'' must be grater than Zero.');
  Result:=BytesOf(TNetEncoding.Base64.Decode(AB64));
end;

class function TCustomBytesFixture.HexToBytes(const AHexStr: string): TBytes;
var
  lLen: NativeUInt;

begin
  Assert.IsNotEmpty(AHexStr, 'Parameter ''AHexStr'' must not be empty.');
  lLen:=Length(AHexStr);
  Assert.IsFalse(Odd(lLen mod 2), 'HEX String must be divisible by 2.');
  lLen:=lLen div 2;
  SetLength(Result, lLen);
  HexToBin(PChar(AHexStr), 0, Result, 0, lLen);
end;

procedure TCustomBytesFixture.InitData(AData: TBytes);
begin
  FData:=AData;
end;

procedure TCustomBytesFixture.InitData(ASize: TIdC_SizeT);
begin
  FData:=TTaurusTLS_Random.PublicRandom.Random(ASize);
end;

class function TCustomBytesFixture.GetBioMemPtr(ABio: ITaurusTLS_Bio; 
  var AMemPtr: pointer): TIdC_INT;
begin
  Assert.IsNotNull(ABio, 'ABio must not be ''nil''.');
  Result:=BIO_get_mem_data(ABio.Bio, AMemPtr);
  Assert.IsTrue(Result >= 0, 'Can not get Data memory pointer form thr ABio.');
end;

function TCustomBytesFixture.GetDataLen: NativeUInt;
begin
  Result:=Length(FData);
end;

function TCustomBytesFixture.CopyData: TBytes;
begin
  Result:=Copy(FData, 0, Length(FData));
end;

class procedure TCustomBytesFixture.TestBytes(AExpected, AData: TBytes);
var
  lExpectedLen, lADataLen: NativeUInt;
begin
  if AExpected = AData then
    Exit; // the same  byte array
  lExpectedLen:=Length(AExpected);
  lADataLen:=Length(AData);
  Assert.AreEqual(lExpectedLen, lADataLen, 'FData and AData lengths are not equal.');
  if lExpectedLen = 0 then
    Exit; // lADataLen = 0 as well.
  Assert.AreEqualMemory(@AExpected[0], @AData[0], lExpectedLen,
    'FData and AData contents are not equal.');
end;

class procedure TCustomBytesFixture.TestData(AExpected: TBytes; AData: pointer;
  ADataLen: NativeUInt);
var
  lExpectedLen: NativeUInt;

begin
  lExpectedLen:=Length(AExpected);
  Assert.AreEqual(lExpectedLen, ADataLen, 'FData and AData lengths are not equal.');
  if lExpectedLen = 0 then
    Exit; // lADataLen = 0 as well.

  if @AExpected[0] = AData then
    Exit; // the same byte array. Address of first element and length are equal.
  Assert.AreEqualMemory(@AExpected[0], AData, lExpectedLen,
    'FData and AData contents are not equal.');
end;

procedure TCustomBytesFixture.TestBytes(AData: TBytes);
begin
  TestBytes(Data, AData);
end;

procedure TCustomBytesFixture.TestData(AData: pointer; ADataLen: NativeUInt);
begin
  TestData(Data, AData, ADataLen);
end;

procedure TCustomBytesFixture.TestBytesIntf(ABytes: ITaurusTLS_Bytes);
begin
  Assert.IsNotNull(ABytes, 'ABytes msut not be ''nil''.');
  TestBytes(ABytes.Bytes);
end;

procedure TCustomBytesFixture.TestBioIntf(ABio: ITaurusTLS_Bio);
var
  lBio: PBIO;
  lBioLen: TIdC_SIZET;
  lMemPtr: Pointer;

begin
  Assert.IsNotNull(ABio, 'ABio msut not be ''nil''.');
  lBIO:=ABio.Bio;
  lBioLen:=BIO_get_mem_data(lBio, lMemPtr);
  Assert.AreEqual<NativeUInt>(DataLen, lBioLen,
    'Unable to extract memory pointer from the lBio');
  TestData(lMemPtr, lBioLen);
end;

{ TPlainBytesFixture }

procedure TBytesFixture.BytesSame(AHexStr: string);
begin
  InitData(HexToBytes(AHexStr));
  TestBytesIntf(TTaurusTLS_Bytes.Create(Data));
end;

procedure TBytesFixture.BytesSame(ASize: NativeUInt);
begin
  InitData(ASize);
  TestBytesIntf(TTaurusTLS_Bytes.Create(Data));
end;

procedure TBytesFixture.BytesCopy(AHexStr: string);
begin
  InitData(HexToBytes(AHexStr));
  TestBytesIntf(TTaurusTLS_Bytes.Create(CopyData));
end;

procedure TBytesFixture.BytesCopy(ASize: NativeUInt);
begin
  InitData(ASize);
  TestBytesIntf(TTaurusTLS_Bytes.Create(CopyData));
end;

procedure TBytesFixture.BioNull(ASize: NativeUInt);
begin
  InitData(0);
  Assert.IsNull((TTaurusTLS_Bytes.Create(Data) as ITaurusTLS_Bytes).NewBio,
    'Excpected ''nil'' Bio.');
end;

procedure TBytesFixture.BioRepeat(ASize, ACount: NativeUInt);
var
  i: NativeUInt;

begin
  InitData(ASize);
  for i:=0 to ACount-1 do
    TestBioIntf((TTaurusTLS_Bytes.Create(Data) as ITaurusTLS_Bytes).NewBio);
end;

procedure TBytesFixture.BioRepeatOneLock(ASize, ACount: NativeUInt);
var
  lBio: ITaurusTLS_Bio;
  i: NativeUInt;

begin
  InitData(ASize);
  lBio:=(TTaurusTLS_Bytes.Create(Data) as ITaurusTLS_Bytes).NewBio;
  TestBioIntf(lBio);
  for i:=0 to ACount-1 do
    TestBioIntf((TTaurusTLS_Bytes.Create(Data) as ITaurusTLS_Bytes).NewBio);
end;

procedure TBytesFixture.BioSame(AHexStr: string);
begin
  InitData(HexToBytes(AHexStr));
  TestBioIntf((TTaurusTLS_Bytes.Create(Data) as ITaurusTLS_Bytes).NewBio);
end;

procedure TBytesFixture.BioSame(ASize: NativeUInt);
begin
  InitData(ASize);
  TestBioIntf((TTaurusTLS_Bytes.Create(Data) as ITaurusTLS_Bytes).NewBio);
end;

procedure TBytesFixture.BioCopy(ASize: NativeUInt);
begin
  InitData(ASize);
  TestBioIntf((TTaurusTLS_Bytes.Create(CopyData) as ITaurusTLS_Bytes).NewBio);
end;

procedure TBytesFixture.BioCopy(AHexStr: string);
begin
  InitData(HexToBytes(AHexStr));
  TestBioIntf((TTaurusTLS_Bytes.Create(CopyData) as ITaurusTLS_Bytes).NewBio);
end;

{ TWipingBytesFixture }

class procedure TWipingBytesFixture.IsEmpty(AData: pointer; ASize: NativeUInt);
var
  lLongPtr: PNativeUInt;
  lBytePtr: PByte;

begin
  if not Assigned(AData) then
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

class procedure TWipingBytesFixture.IsEmpty(ABio: PBIO; AExpectedLen: NativeUInt);
var
  lMemPtr: pointer;
  lLen: TIdC_INT;

begin
  Assert.IsNotNull(ABio, 'ABio msut not be ''nil''.');
  lLen:=BIO_get_mem_data(ABio, lMemPtr);
  Assert.AreEqual<NativeUInt>(AExpectedLen, lLen,
    'ABio length is not equal AExpectedLen.');
  IsEmpty(lMemPtr, AExpectedLen);
end;

class procedure TWipingBytesFixture.IsEmpty(ABytes: TBytes);
var
  lLen: NativeUInt;

begin
  lLen:=Length(ABytes);
  if lLen = 0 then
    Exit;
  IsEmpty(@ABytes[0], lLen);
end;

procedure TWipingBytesFixture.TestWipeBytes;
var
  lBytes: ITaurusTLS_Bytes;
  lData: TBytes;

begin
  lBytes:=TTaurusTLS_WipingBytes.Create(CopyData);
  TestBytesIntf(lBytes);
  lData:=lBytes.Bytes;
  lBytes:=nil;
  IsEmpty(lData);
end;

procedure TWipingBytesFixture.WipeBytes(ASize: NativeUInt);
begin
  InitData(ASize);
  TestWipeBytes;
end;

procedure TWipingBytesFixture.WipeBytes(AHexStr: string);
begin
  InitData(HexToBytes(AHexStr));
  TestWipeBytes;
end;

procedure TWipingBytesFixture.TestWipeBio;
var
  lBytes: ITaurusTLS_Bytes;
  lBio: ITaurusTLS_Bio;
  lBioPtr: pointer;

begin
  lBytes:=TTaurusTLS_WipingBytes.Create(CopyData);
  lBio:=lBytes.NewBio;
  TestBioIntf(lBio);
  GetBioMemPtr(lBio, lBioPtr);
  lBytes:=nil;
  TestBioIntf(lBio);
  lBio:=nil;
  IsEmpty(lBioPtr, DataLen);
end;

procedure TWipingBytesFixture.WipeBio(ASize: NativeUInt);
begin
  InitData(ASize);
  TestWipeBio;
end;

procedure TWipingBytesFixture.WipeBio(AHexStr: string);
begin
  InitData(HexToBytes(AHexStr));
  TestWipeBio;
end;

{ TEnryptedBytesFixture }

procedure TEnryptedBytesFixture.IsEncrypted(AData: pointer;
  ADataLen: NativeUInt);
var
  lLen: NativeUInt;
  
begin
  lLen:=DataLen;
  Assert.AreNotEqual<NativeUInt>(0, lLen, 'Data is not initialized.');
  Assert.IsNotNull(AData, 'AData must be not ''nil''.');
  Assert.AreNotEqual<NativeUInt>(0, lLen, 'ADataLen must be greater than Zero.');
  if lLen > ADataLen then
    lLen:=ADataLen;
  Assert.AreNotEqualMemory(@FData[0], AData, lLen,
    'Plain and Encrypted data looks the same...');
end;

procedure TEnryptedBytesFixture.IsEncrypted(const ABytes: TBytes);
var
  lLen: NativeUInt;
  
begin
  lLen:=Length(ABytes);
  Assert.AreNotEqual<NativeUInt>(0, lLen, 'ABytes should not be an empty array.');
  IsEncrypted(@ABytes[0], lLen);
end;

procedure TEnryptedBytesFixture.IsUnEncrypted(ABio: ITaurusTLS_Bio);
var
  lMemPtr: Pointer;
  lLen: TIdC_INT;
  
begin
  lLen:=GetBioMemPtr(ABio, lMemPtr);
  TestData(lMemPtr, lLen);
end;

class function TEnryptedBytesFixture.NewBytes(
  AHexStr: string): ITaurusTLS_Bytes;
begin
  Result:=NewBytes(HexToBytes(AHexStr));
end;

class function TEnryptedBytesFixture.NewBytes(ABytes: TBytes): ITaurusTLS_Bytes;
begin
  Result:=TTaurusTLS_EncryptedBytes.Create(ABytes,
    TTaurusTLS_SimpleAESEncryptor.Create(aks192, emOFB));
end;

procedure TEnryptedBytesFixture.Bytes(ASize: NativeUInt);
var
  lBytes: ITaurusTLS_Bytes;
  
begin
  InitData(ASize);
  lBytes:=NewBytes(CopyData);
  IsEncrypted(lBytes.Bytes);
end;

procedure TEnryptedBytesFixture.Bytes(AHexStr: string);
var
  lBytes: ITaurusTLS_Bytes;
  
begin
  InitData(HexToBytes(AHexStr));
  lBytes:=NewBytes(CopyData);
  IsEncrypted(lBytes.Bytes);
end;

procedure TEnryptedBytesFixture.Bio(ASize: NativeUInt);
var
  lBytes: ITaurusTLS_Bytes;
  lBio: ITaurusTLS_Bio;
  
begin
  InitData(ASize);
  lBytes:=NewBytes(CopyData);
  lBio:=lBytes.NewBio;
  IsUnencrypted(lBio);
end;

procedure TEnryptedBytesFixture.BioRepeat(ASize, ACount: NativeUInt);
var
  lBytes: ITaurusTLS_Bytes;
  i: NativeUInt;

begin
  InitData(ASize);
  lBytes:=NewBytes(CopyData);
  for i:=0 to ACount-1 do
    TestBioIntf(lBytes.NewBio);
end;

procedure TEnryptedBytesFixture.BioRepeatOneLock(ASize, ACount: NativeUInt);
var
  lBytes: ITaurusTLS_Bytes;
  lBio: ITaurusTLS_Bio;
  i: NativeUInt;

begin
  InitData(ASize);
  lBytes:=NewBytes(CopyData);
  lBio:=lBytes.NewBio;
  TestBioIntf(lBio);
  for i:=0 to ACount-1 do
    TestBioIntf(lBytes.NewBio);
end;

procedure TEnryptedBytesFixture.Bio(AHexStr: string);
var
  lBytes: ITaurusTLS_Bytes;
  lBio: ITaurusTLS_Bio;

begin
  InitData(HexToBytes(AHexStr));
  lBytes:=NewBytes(CopyData);
  lBio:=lBytes.NewBio;
  IsUnencrypted(lBio);
end;

{ TSimpleAESFactoryFixture }

procedure TSimpleAESFactoryFixture.KeySizeNameAll;
var
  i: TTaurusTLS_AESKeySize;

begin
  for i:=Low(TTaurusTLS_AESKeySize) to High(TTaurusTLS_AESKeySize) do
    Assert.AreEqual(TSimpleAESEnums.KeySizeName[i],
      TTaurusTLS_SimpleAESFactory.KeySizeName[i], 'KeySize Name is invalid.');
end;

procedure TSimpleAESFactoryFixture.EncodeModeNameAll;
var
  i: TTaurusTLS_EncodeMode;

begin
  for i:=Low(TTaurusTLS_EncodeMode) to High(TTaurusTLS_EncodeMode) do
    Assert.AreEqual(TSimpleAESEnums.EncodeModeName[i],
      TTaurusTLS_SimpleAESFactory.EncodeModeName[i], 'EncodeMode Name is invalid.');
end;

procedure TSimpleAESFactoryFixture.NewEncryptor;
var
  lKeySize: TTaurusTLS_AESKeySize;
  lEncodeMode: TTaurusTLS_EncodeMode;
  lEncryptor: TTaurusTLS_CustomEncryptor;

begin
  lKeySize:=TSimpleAESEnums.KeySize[AKeySizeName];
  lEncodeMode:=TSimpleAESEnums.EncodeMode[AEncodeMode];
  lEncryptor:=nil;
  try
    lEncryptor:=TTaurusTLS_SimpleAESFactory.NewEncryptor(lKeySize, lEncodeMode);
    Assert.IsNotNull(lEncryptor, 'NewEncryptor failed to create Encryptor instance');
    Assert.InheritsFrom(lEncryptor.ClassType, TTaurusTLS_SimpleAESEncryptor,
      'Encyptor instance inherits from a unexpected class.');
    Assert.AreEqual(TSimpleAESEnums.KeySizeInt[lKeySize], lEncryptor.KeyLen*8,
      'Invalid Encryptor Cipher KeyLen.');
    Assert.AreEqual<TIdC_INT>(TIdC_INT(lEncodeMode), lEncryptor.Mode,
      'Invalid Encryptor Cipher EncodeMode.');
  finally
    lEncryptor.Free;
  end;
end;

procedure TSimpleAESFactoryFixture.SetDefaultCipher(AKeySizeName,
  AEncodeMode: string);
var
  lKeySize: TTaurusTLS_AESKeySize;
  lEncodeMode: TTaurusTLS_EncodeMode;

begin
  lKeySize:=TSimpleAESEnums.KeySize[AKeySizeName];
  lEncodeMode:=TSimpleAESEnums.EncodeMode[AEncodeMode];
  TTaurusTLS_SimpleAESFactory.SetDefaultCipher(lKeySize, lEncodeMode);
  Assert.AreEqual<TTaurusTLS_AESKeySize>(lKeySize,
    TTaurusTLS_SimpleAESFactory.DefaultKeySize, 'DefaultKeySize was not updated.');
  Assert.AreEqual<TTaurusTLS_EncodeMode>(lEncodeMode,
    TTaurusTLS_SimpleAESFactory.DefaultEncodeMode, 'DefaultKeySize was not updated.');
end;

procedure TSimpleAESFactoryFixture.NewEncyptorDefault(AKeySizeName,
  AEncodeMode: string);
var
  lEncryptor: TTaurusTLS_CustomEncryptor;
  lKeySize: TTaurusTLS_AESKeySize;
  lEncodeMode: TTaurusTLS_EncodeMode;

begin
  lEncryptor:=nil;
  try
    SetDefaultCipher(AKeySizeName, AEncodeMode);
    lEncryptor:=TTaurusTLS_SimpleAESFactory.NewEncryptor;
    lKeySize:=TSimpleAESEnums.KeySize[AKeySizeName];
    lEncodeMode:=TSimpleAESEnums.EncodeMode[AEncodeMode];
    Assert.AreEqual(TSimpleAESEnums.KeySizeInt[lKeySize], lEncryptor.KeyLen*8,
      'Invalid Encryptor Cipher KeyLen.');
    Assert.AreEqual<TIdC_INT>(TIdC_INT(lEncodeMode), lEncryptor.Mode,
      'Invalid Encryptor Cipher EncodeMode.');
  finally
    lEncryptor.Free;
  end;
end;

{ TCustomBytesHelperFixture }

procedure TBytesHelperFixture.LoadFromBytes(AHexStr: string);
var
  lIBytes: ITaurusTLS_Bytes;
  lSrc, lBytes: TBytes;
  lLen: NativeUInt;

begin
  lSrc:=HexToBytes(AHexStr);
  lLen:=Length(lSrc);
  Assert.AreNotEqual<NativeUInt>(0, lLen, 'AData is empty string.');
  lIBytes:=TTaurusTLS_Bytes.LoadFromBytes<TTaurusTLS_Bytes>(lSrc);
  lBytes:=lIBytes.Bytes;
  Assert.AreEqual<NativeUInt>(lLen, Length(lBytes),
    'Length of AData and lBytes are not equal.');
  TestData(lSrc, lBytes, lLen);
end;

procedure TBytesHelperFixture.LoadFromPAnsiStr(AData: RawByteString);
var
  lIBytes: ITaurusTLS_Bytes;
  lBytes: TBytes;
  lLen: NativeUInt;

begin
  lLen:=Length(AData);
  Assert.AreNotEqual<NativeUInt>(0, lLen, 'AData is empty string.');
  lIBytes:=TTaurusTLS_Bytes.LoadFromString<TTaurusTLS_Bytes>(PIdAnsiChar(AData));
  lBytes:=lIBytes.Bytes;
  Assert.AreEqual<NativeUInt>(lLen, Length(lBytes),
    'Length of AData and lBytes are not equal.');
  TestData(lBytes, @AData[1], lLen);
end;

procedure TBytesHelperFixture.LoadFromString(AData: string);
var
  lIBytes: ITaurusTLS_Bytes;
  lBytes: TBytes;
  lLen: NativeUInt;
  lBytesStr: string;

begin
  lLen:=Length(AData);
  Assert.AreNotEqual<NativeUInt>(0, lLen, 'AData is empty string.');
  lIBytes:=TTaurusTLS_Bytes.LoadFromString<TTaurusTLS_Bytes>(AData);
  lBytes:=lIBytes.Bytes;
  Assert.AreEqual<NativeUInt>(lLen, Length(lBytes),
    'Length of AData and lBytes are not equal.');
  lBytesStr:=StringOf(lBytes);
  Assert.AreEqual(AData, lBytesStr, 'AData and lBytes are not equal.')
end;

procedure TBytesHelperFixture.LoadFromMemStream(AHexStr: string);
var
  lStream: TMemoryStream;
  lSrcBytes, lBytes: TBytes;
  lIBytes: ITaurusTLS_Bytes;

begin
  lSrcBytes:=HexToBytes(AHexStr);
  lStream:=nil;
  try
    lStream:=TMemoryStream.Create;
    lStream.Write(lSrcBytes, Length(lSrcBytes));

    lIBytes:=TTaurusTLS_Bytes.LoadFromStream<TTaurusTLS_Bytes>(lStream);
    Assert.IsNotNull(lIBytes, 'Failed to create a ITaurusTLS_Bytes instance '+
      'from a TStream.');
    lBytes:=lIBytes.Bytes;
    TestBytes(lSrcBytes, lBytes);
  finally
    lStream.Free;
  end;
end;

procedure TBytesHelperFixture.LoadFromByteStream(AHexStr: string);
var
  lStream: TBytesStream;
  lSrcBytes, lBytes: TBytes;
  lIBytes: ITaurusTLS_Bytes;

begin
  lSrcBytes:=HexToBytes(AHexStr);
  lStream:=nil;
  try
    lStream:=TBytesStream.Create(lSrcBytes);
    lStream.Write(lSrcBytes, Length(lSrcBytes));

    lIBytes:=TTaurusTLS_Bytes.LoadFromStream<TTaurusTLS_Bytes>(lStream);
    Assert.IsNotNull(lIBytes, 'Failed to create a ITaurusTLS_Bytes instance '+
      'from a TStream.');
    lBytes:=lIBytes.Bytes;
    Assert.AreEqual(@lSrcBytes[0], @lBytes[0], 'lSrcBytes and lBytes are not equal. ');
  finally
    lStream.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCipherFixture);
  TDUnitX.RegisterTestFixture(TSimpleAESEncryptorFixture);
  TDUnitX.RegisterTestFixture(TBytesFixture);
  TDUnitX.RegisterTestFixture(TWipingBytesFixture);
  TDUnitX.RegisterTestFixture(TEnryptedBytesFixture);
  TDUnitX.RegisterTestFixture(TSimpleAESFactoryFixture);
  TDUnitX.RegisterTestFixture(TBytesHelperFixture);
  TDUnitX.RegisterTestFixture(TEncyptedBytesHelperFixture);

end.
