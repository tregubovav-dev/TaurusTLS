{******************************************************************************}
{*  TaurusTLS                                                                 *}
{*           https://github.com/JPeterMugaas/TaurusTLS                        *}
{*                                                                            *}
{*  Copyright (c) 2024 - 2025 TaurusTLS Developers, All Rights Reserved       *}
{*                                                                            *}
{* Portions of this software are Copyright (c) 1993 – 2018,                   *}
{* Chad Z. Hower (Kudzu) and the Indy Pit Crew – http://www.IndyProject.org/  *}
{******************************************************************************}
{$I TaurusTLSCompilerDefines.inc}
///   <summary>
///   Implements various helpers classes for classes used in the
///   TaurusTLS_SSLContainers unit.
///   </summary>

unit TaurusTLS_SSLContainersHelpers;
{$I TaurusTLSLinkDefines.inc}

interface

uses
  Classes, SysUtils,
  IdGlobal, IdCTypes;

type
  ///  <summary>
  ///  Implements set of methods to <c>wipe out</c> (fill with zeros)
  ///  various data types
  ///  </summary>
  TWiper = class
  protected
    class procedure Wipe(APtr: pointer; ALen: TIdC_SIZET);
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    ///  <summary>
    ///  Fills with <c>zeros</c> variable of
    ///  <see cref="RawByteString" /> type.
    ///  </summary>
    ///  <param name="AStr">
    ///  A string of <see cref="RawByteString" /> type.
    ///  </param>
    class procedure Wipe(var AStr: RawByteString);
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  <summary>
    ///  Fills with <c>zeros</c> variable of
    ///  <see cref="AnsiString" /> type.
    ///  </summary>
    ///  <param name="AStr">
    ///  A string of <see cref="AnsiString" /> type.
    ///  </param>
    class procedure Wipe(var AStr: AnsiString);
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  <summary>
    ///  Fills with <c>zeros</c> variable of
    ///  <see cref="UTF8String" /> type.
    ///  </summary>
    ///  <param name="AStr">
    ///  A string of <see cref="UTF8String" /> type.
    ///  </param>
    class procedure Wipe(var AStr: UTF8String);
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  <summary>
    ///  Fills with <c>zeros</c> variable of
    ///  <see cref="UnicodeString" /> type.
    ///  </summary>
    ///  <param name="AStr">
    ///  A string of <see cref="UnicodeString" /> type.
    ///  </param>
    class procedure Wipe(var AStr: UnicodeString);
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  <summary>
    ///  Fills with <c>zeros</c> variable of
    ///  <see cref="TBytes" /> type.
    ///  </summary>
    ///  <param name="AData">
    ///  An <c>array of bytes</c> variable (<see cref="TBytes" /> type).
    ///  </param>
    class procedure Wipe(var AData: TBytes);
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  ///  <summary>
  ///  Adds set of methods to initialze instances of <see cref="TBytes" />
  ///  with various sources
  ///  </summary>
  TBytesFactory = class
  protected
    class function CreateUnicode(const AStr: UnicodeString;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CreateAnsi(const AStr: AnsiString;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CreateUTF8(const AStr: UTF8String;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function UnicodeToUTF8(AStr: PWideChar; ALen: integer;
      AWithTrailingNull: boolean): TBytes; static;
{$IFDEF DCC}
    class function AnsiToUTF8(AStr: PAnsiChar; ALen: integer;
      AWithTrailingNull: boolean; cp: cardinal; AWipe: boolean = False): TBytes;
{$ELSE}
    class function AnsiToUTF8(AStr: PAnsiChar; ALen: integer;
      AWithTrailingNull: boolean; cp: TSystemCodePage ;
      AWipe: boolean = False): TBytes;
{$ENDIF}
      static;
    ///  <summary>
    ///  Creates instance of <see cref="TBytes" /> and copies number of
    ///  <c>ALen</c> bytes from the memory pointed with <c>AData</c>
    ///  into created <see cref="TBytes" /> array.
    ///  </summary>
    ///  <param name="AData">
    ///  Pointer to the memory region which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  </param>
    ///  <param name="ALen">
    ///  Size of created array and <see cref="TBytes" /> number of bytes
    ///  copied to it
    ///  </param>
    ///  <remarks>
    ///  Function returns empty array with length of <c>ALen</c> parameter
    ///  when <c>AData</c> parameter is <c>nil</c>
    ///  </remarks>
    class function Create(const AData: pointer; ALen: NativeUInt): TBytes;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, converts content of
    ///  <c>AStr</c> string to <see cref="UnicodeString" /> and copies its
    ///  conntent into created <see cref="TBytes" /> array.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="UnicodeString" /> which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="UnicodeString" />.
    ///  </param>
    class function Create(const AStr: UnicodeString; AWithTrailingNull: boolean): TBytes;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, copies conntent of <c>AStr</c>
    ///  into created <see cref="TBytes" /> array.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="AnsiString" /> which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="AnsiString" />.
    ///  </param>
    class function Create(const AStr: AnsiString; AWithTrailingNull: boolean): TBytes;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" /> andcopies conntent of <c>AStr</c>
    ///  into created <see cref="TBytes" /> array.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="UTF8String" /> which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="UTF8String" />.
    ///  </param>
    class function Create(const AStr: UTF8String; AWithTrailingNull: boolean): TBytes;
      overload; static; {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  Creates instance of <see cref="TBytes" /> with lenght of <c>ACount</c>,
    ///  copies number of <c>bytes</c> from the <c>AStream</c> into
    ///  created <see cref="TBytes" /> array.
    ///  <para>
    ///  Method copies <c>ACount</c> bytes from the stream specified
    ///  by <c>AStream</c> into the <c>array of bytes</c>. It then moves
    ///  the current position by <c>ACount</c> bytes and returns
    ///  the instance of <see cref="TBytes" />.
    ///  </para>
    ///  <para>
    ///  If <c>ACount</c> is 0, method sets <c>AStream</c> position to 0 before reading
    ///  and then copies the entire contents of <c>AStream</c> into
    ///  the <c>array of bytes</c>.
    ///  If <c>ACount</c> is greater than 0, method reads from the current position
    ///  in <c>AStream</c>.
    ///  </para>
    ///  </summary>
    ///  <param name="AStream">
    ///  A <see cref="TStream" /> which content is copied to the
    ///  the <c>array of bytes</c>.
    ///  </param>
    ///  <param name="ACount">
    ///  Number of <c>bytes</c> to copy from the <c>AStream</c>.
    ///  </param>
    class function Create(const AStream: TStream; ACount: NativeUInt): TBytes;
      overload; static;

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, converts content of
    ///  <c>AStr</c> string to <see cref="UnicodeString" />, copies its
    ///  conntent into created <see cref="TBytes" /> array, and fills
    ///  the origianl string with <c>zeros</c>.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="UnicodeString" /> which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  The <c>AStr</c> will be filled with <c>zeros</c> after metod returns.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="UnicodeString" />.
    ///  </param>
    class function CreateAndWipe(var AStr: UnicodeString;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, copies conntent of <c>AStr</c>
    ///  into created <see cref="TBytes" /> array, and fills
    ///  the origianl string with <c>zeros</c>.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="RawByteString" /> which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  The <c>AStr</c> will be filled with <c>zeros</c> after metod returns.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="AnsiString" />.
    ///  </param>
    class function CreateAndWipe(var AStr: RawByteString;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  Creates instance of <see cref="TBytes" />, copies conntent of <c>AStr</c>
    ///  into created <see cref="TBytes" /> array, and fills
    ///  the origianl string with <c>zeros</c>.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="AnsiString" /> which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  The <c>AStr</c> will be filled with <c>zeros</c> after metod returns.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="AnsiString" />.
    ///  </param>
    class function CreateAndWipe(var AStr: AnsiString;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  Creates instance of <see cref="TBytes" />, copies conntent of <c>AStr</c>
    ///  into created <see cref="TBytes" /> array, and fills
    ///  the origianl string with <c>zeros</c>.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="UTF8String" /> which will be copied to the <see cref="TBytes" />
    ///  array.
    ///  The <c>AStr</c> will be filled with <c>zeros</c> after metod returns.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="UTF8String" />.
    ///  </param>
    class function CreateAndWipe(var AStr: UTF8String;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  Creates instance of <see cref="TBytes" /> with lenght of <c>ACount</c>,
    ///  copies number of <c>bytes</c> from the <c>AStream</c> into
    ///  created <see cref="TBytes" /> array.
    ///  This method fills the memory buffer of instance
    ///  of <see cref="TMemoryStream" /> class and its descendant(s) with <c>zeros</c>.
    ///  <para>
    ///  Method copies <c>ACount</c> bytes from the stream specified
    ///  by <c>AStream</c> into the <c>array of bytes</c>. It then moves
    ///  the current position by <c>ACount</c> bytes and returns it.
    ///  If <c>AStream</c> as the instance of <see cref="TMemoryStream" />,
    ///  method fills see cref="TMemoryStream.Memory" /> with zeros
    ///  <c>ACount</c> of bytes starting the initial <c>AStream.Position</c>.
    ///  </para>
    ///  <para>
    ///  If <c>ACount</c> is 0, method sets <c>AStream</c> position to 0 before reading
    ///  and then copies the entire contents of <c>AStream</c> into
    ///  the <c>array of bytes</c> then and returns it.
    ///  If <c>AStream</c> as the instance of <see cref="TMemoryStream" />,
    ///  method fills entire <see cref="TMemoryStream.Memory" /> with zeros.
    ///  If <c>ACount</c> is greater than 0, method reads from the current position
    ///  in <c>AStream</c>.
    ///  </para>
    ///  </summary>
    ///  <param name="AStream">
    ///  A <see cref="TStream" /> which content is copied to the
    ///  the <c>array of bytes</c>.
    ///  </param>
    ///  <param name="ACount">
    ///  Number of <c>bytes</c> to copy from the <c>AStream</c>.
    ///  </param>
    class function CreateAndWipeMemBuf(const AStream: TStream;
      ACount: NativeUInt): TBytes; overload; static;

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, converts content of
    ///  <c>AStr</c> string to <see cref="UTF8String" /> and copies its
    ///  conntent into created <see cref="TBytes" /> array.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="UnicodeString" /> which will be converted to <c>UTF8</c>
    ///  format and stored it in the the <see cref="TBytes" /> array.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="UnicodeString" />.
    ///  </param>
    class function CreateAsUTF8(const AStr: UnicodeString;
      AWithTrailingNull: boolean): TBytes; overload; static;

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, converts content of
    ///  <c>AStr</c> string to <see cref="UTF8String" /> and copies its
    ///  conntent into created <see cref="TBytes" /> array.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="AnsiString" /> which will be converted to <c>UTF8</c>
    ///  format using <c>CodePage</c> in parameter <c>cp</c>,
    ///  and stored it in the the <see cref="TBytes" /> array.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="AnsiString" />.
    ///  </param>
    ///  <param name="cp">
    ///  Indicates to the <c>AStr CodePage</c>.
    ///  </param>
{$IFDEF DCC}
    class function CreateAsUTF8(const AStr: AnsiString;
      AWithTrailingNull: boolean): TBytes; overload; static;
{$ELSE}
    class function CreateAsUTF8(const AStr: AnsiString;
      AWithTrailingNull: boolean): TBytes; overload; static;
{$ENDIF}

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, converts content of
    ///  <c>AStr</c> string to <see cref="UTF8String" />, copies its
    ///  conntent into created <see cref="TBytes" /> array, and fills
    ///  the origianl string with <c>zeros</c>.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="UnicodeString" /> which will be converted to <c>UTF8</c>
    ///  format and stored it in the the <see cref="TBytes" /> array.
    ///  The <c>AStr</c> will be filled with <c>zeros</c> after metod returns.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="UnicodeString" />.
    ///  </param>
    class function CreateAsUTF8AndWipe(var AStr: UnicodeString;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}

    ///  <summary>
    ///  Creates instance of <see cref="TBytes" />, converts content of
    ///  <c>AStr</c> string to <see cref="UTF8String" />, copies its
    ///  conntent into created <see cref="TBytes" /> array, and fills
    ///  the origianl string with <c>zeros</c>.
    ///  </summary>
    ///  <param name="AStr">
    ///  A <see cref="AnsiString" /> which will be converted to <c>UTF8</c>
    ///  format and stored it in the the <see cref="TBytes" /> array.
    ///  The <c>AStr</c> will be filled with <c>zeros</c> after metod returns.
    ///  </param>
    ///  <param name="AWithTrailingNull">
    ///  Indicates whether to add a zero-value byte at the end of <see cref="TBytes" />
    ///  array. Adding this byte allows to represent <see cref="TBytes" /> array
    ///  as null-terminated <see cref="AnsiString" />.
    ///  </param>
    class function CreateAsUTF8AndWipe(var AStr: AnsiString;
      AWithTrailingNull: boolean): TBytes; overload; static;
      {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

implementation

uses
  TaurusTLS_ResourceStrings;

type
  TStreamHelper = class helper for TStream
    procedure WipeMemoryData(AStart, ACount: NativeUInt);
  end;

{ TStreamHelper }

procedure TStreamHelper.WipeMemoryData(AStart, ACount: NativeUInt);
var
  lStartPos: PByte;

begin
  if not (Self is TMemoryStream) then
    Exit;
  if (AStart+ACount > Size) then
    raise EStreamError.Create(RSMSG_TMemoryStreamWipeOutOfRange);
  lStartPos:=PByte(TMemoryStream(Self).Memory)+AStart;
  TWiper.Wipe(lStartPos, ACount);
end;

{ TWiper }

class procedure TWiper.Wipe(APtr: pointer; ALen: TIdC_SIZET);
begin
  if ALen > 0 then
    FillChar(APtr^, ALen, $0);
end;

class procedure TWiper.Wipe(var AStr: RawByteString);
begin
  Wipe(PAnsiChar(AStr), Length(AStr)*SizeOf(AnsiChar));
end;

class procedure TWiper.Wipe(var AStr: UTF8String);
begin
  Wipe(PAnsiChar(AStr), Length(AStr)*SizeOf(AnsiChar));
end;

class procedure TWiper.Wipe(var AStr: AnsiString);
begin
  Wipe(RawByteString(AStr));
end;

class procedure TWiper.Wipe(var AStr: UnicodeString);
begin
  Wipe(PWideChar(AStr), Length(AStr)*SizeOf(WideChar));
end;

class procedure TWiper.Wipe(var AData: TBytes);
var
  lLen: NativeUInt;

begin
  lLen:=Length(AData);
  if lLen > 0 then
    Wipe(PByte(AData), lLen);
end;

{ TBytesHelper }

class function TBytesFactory.CreateUnicode(const AStr: UnicodeString;
  AWithTrailingNull: boolean): TBytes;
var
  lLen: NativeUInt;
  lPtr: pointer;

begin
  lLen:=Length(AStr);
  if lLen = 0  then
    lPtr:=nil
  else
    lPtr:=PWideChar(Astr);
  if AWithTrailingNull then
    Inc(lLen);
  Result:=Create(lPtr, lLen*SizeOf(WideChar));
end;

class function TBytesFactory.CreateAnsi(const AStr: AnsiString;
  AWithTrailingNull: boolean): TBytes;
var
  lLen: NativeUInt;
  lPtr: pointer;

begin
  lLen:=Length(AStr);
  if lLen = 0  then
    lPtr:=nil
  else
    lPtr:=PAnsiChar(Astr);
  if AWithTrailingNull then
    Inc(lLen);
  Result:=Create(lPtr, lLen*SizeOf(AnsiChar));
end;

// we have to add CreateUtf8 explicitly to avoid implicit string conversion
// in AnsiString(var_UTF8String).
class function TBytesFactory.CreateUTF8(const AStr: UTF8String;
  AWithTrailingNull: boolean): TBytes;
var
  lLen: NativeUInt;
  lPtr: pointer;

begin
  lLen:=Length(AStr);
  if lLen = 0  then
    lPtr:=nil
  else
    lPtr:=PAnsiChar(Astr);
  if AWithTrailingNull then
    Inc(lLen);
  Result:=Create(lPtr, lLen*SizeOf(AnsiChar));
end;

class function TBytesFactory.UnicodeToUTF8(AStr: PWideChar; ALen: integer;
  AWithTrailingNull: boolean): TBytes;
{$IFDEF DCC}
var
  lULen, lAdjLen: integer;

begin
  if ALen > 0 then
  begin
    lULen := LocaleCharsFromUnicode(CP_UTF8, 0, AStr, ALen, nil, 0, nil, nil);
    if lULen = 0 then
      raise EEncodingError.Create(RSMSG_UTF8NoMapping);
  end
  else
    lULen:=0;
  lAdjLen := lULen;
  if AWithTrailingNull then
    Inc(lAdjLen);
  SetLength(Result, lAdjLen);
  if lULen > 0 then
    LocaleCharsFromUnicode(CP_UTF8, 0, AStr, ALen, PAnsiChar(Result),
      lAdjLen, nil, nil);
end;
{$ELSE}
var
  lTmp: RawByteString;
  lLen, lAdjLen: SizeUInt;

begin
//  if ALen <= 0 then
//    Exit;
  lLen:=ALen*SizeOf(WideChar);
{$WARN 5091 off}
  WideStringManager.Unicode2AnsiMoveProc(AStr, lTmp, CP_UTF8, ALen);
{$WARN 5091 on}
  try
    lAdjLen:=lLen;
    if AWithTrailingNull then
       Inc(lAdjLen, SizeOf(WideChar));
{$WARN 5093 off}
    SetLength(Result, lAdjLen);
{$WARN 5093 on}
    Move(PChar(lTmp)^, Result[0], lLen);
  finally
    TWiper.Wipe(AStr, lLen);
  end;
end;
{$ENDIF}

{$IFDEF DCC}
class function TBytesFactory.AnsiToUTF8(AStr: PAnsiChar; ALen: integer;
  AWithTrailingNull: boolean; cp: cardinal; AWipe: boolean): TBytes;
var
  lWLen, lAdjLen: integer;
  lTmp: TBytes;

begin
  if cp = CP_UTF8 then
  begin
    lAdjLen:=ALen;
    if AWithTrailingNull then
      Inc(lAdjLen);
    SetLength(Result, lAdjLen);
    if ALen > 0 then
      Move(AStr^, Result[0], ALen);
  end
  else
  if ALen = 0 then
  begin
    if AWithTrailingNull then
      SetLength(Result, 1)
  end
  else
  begin
    lWLen:=UnicodeFromLocaleChars(cp, 0, AStr, ALen, NIL, 0);
    if lWLen = 0 then
      raise EEncodingError.Create(RSMSG_UnicodeNoMapping)
    else
    begin
      SetLength(lTmp, lWLen*SizeOf(WideChar));
      try
        UnicodeFromLocaleChars(cp, 0, AStr, ALen, PWideChar(PByte(lTmp)), lWLen);
        Result:=UnicodeToUTF8(PWideChar(PByte(lTmp)), lWLen, AWithTrailingNull);
      finally
        if AWipe then
          TWiper.Wipe(lTmp);
      end;
    end;
  end;
end;
{$ELSE}
class function TBytesFactory.AnsiToUTF8(AStr: PAnsiChar; ALen: integer;
  AWithTrailingNull: boolean; cp: TSystemCodePage ; AWipe: boolean): TBytes;
var
  lULen: integer;
  lUStr: UnicodeString;

begin
{$WARN 5091 off}
  widestringmanager.Ansi2UnicodeMoveProc(AStr, cp, lUStr, ALen);
{$WARN 5091 on}
  try
    lULen:=Length(lUStr);
    Result:=UnicodeToUTF8(PWideChar(lUstr), lULen, AWithTrailingNull);
  finally
    if AWipe then
      TWiper.Wipe(lUStr);
  end;
end;
{$ENDIF}

class function TBytesFactory.Create(const AData: pointer;
  ALen: NativeUInt): TBytes;
begin
{$IFDEF FPC}
  {$WARN 5093 off}
{$ENDIF}
  SetLength(Result, ALen);
{$IFDEF FPC}
  {$WARN 5093 on}
{$ENDIF}
  if Assigned(AData) then
    Move(AData^, Result[0], ALen);
end;

class function TBytesFactory.Create(const AStr: UnicodeString;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=CreateUnicode(AStr, AWithTrailingNull);
end;

class function TBytesFactory.Create(const AStr: AnsiString;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=CreateAnsi(AStr, AWithTrailingNull);
end;

class function TBytesFactory.Create(const AStr: UTF8String;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=CreateUTF8(AStr, AWithTrailingNull);
end;

class function TBytesFactory.Create(const AStream: TStream;
  ACount: NativeUInt): TBytes;
var
  lStream: TBytesStream;

begin
  if (ACount = 0) or (not Assigned(AStream)) then
    Exit;

{$IFDEF FPC}
  {$WARN 5093 off}
{$ENDIF}
  SetLength(Result, ACount);
{$IFDEF FPC}
  {$WARN 5093 on}
{$ENDIF}
  lStream:=TBytesStream.Create(Result);
  try
    try
      lStream.CopyFrom(AStream, ACount);
      Result:=lStream.Bytes;
    finally
      lStream.Free;
    end;
  except
    SetLength(Result, 0);
    raise;
  end;
end;

class function TBytesFactory.CreateAndWipe(var AStr: UnicodeString;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=Create(AStr, AWithTrailingNull);
  TWiper.Wipe(AStr);
end;

class function TBytesFactory.CreateAndWipe(var AStr: RawByteString;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=Create(AnsiString(AStr), AWithTrailingNull);
  TWiper.Wipe(AStr);
end;

class function TBytesFactory.CreateAndWipe(var AStr: AnsiString;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=CreateAndWipe(RawByteString(AStr), AWithTrailingNull);
end;

class function TBytesFactory.CreateAndWipe(var AStr: UTF8String;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=Create(Astr, AWithTrailingNull);
  TWiper.Wipe(AStr);
end;

class function TBytesFactory.CreateAndWipeMemBuf(const AStream: TStream;
  ACount: NativeUInt): TBytes;
var
  lPos: Int64;
  lCount: NativeUInt;

begin
  if ACount = 0 then
  begin
    lPos:=0;
    lCount:=ACount;
  end
  else
  begin
    lPos:=AStream.Position;
    lCount:=AStream.Size-lPos;
  end;
  Result:=Create(AStream, ACount);
  AStream.WipeMemoryData(lPos, lCount);
end;

class function TBytesFactory.CreateAsUTF8(
  const AStr: UnicodeString; AWithTrailingNull: boolean): TBytes;
begin
  Result:=UnicodeToUTF8(PWideChar(AStr), Length(AStr), AWithTrailingNull);
end;

{$IFDEF DCC}
class function TBytesFactory.CreateAsUTF8(const AStr: AnsiString;
  AWithTrailingNull: boolean): TBytes;
{$ELSE}
class function TBytesFactory.CreateAsUTF8(const AStr: AnsiString;
  AWithTrailingNull: boolean): TBytes;
{$ENDIF}
begin
  Result:=AnsiToUTF8(PAnsiChar(AStr), Length(AStr), AWithTrailingNull,
    StringCodePage(AStr), False);
end;

class function TBytesFactory.CreateAsUTF8AndWipe(var AStr: UnicodeString;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=CreateAsUTF8(AStr, AWithTrailingNull);
  TWiper.Wipe(AStr);
end;

class function TBytesFactory.CreateAsUTF8AndWipe(var AStr: AnsiString;
  AWithTrailingNull: boolean): TBytes;
begin
  Result:=CreateAsUTF8(AStr, AWithTrailingNull);
  TWiper.Wipe(AStr);
end;

end.
