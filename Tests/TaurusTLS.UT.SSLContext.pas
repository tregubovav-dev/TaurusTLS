unit TaurusTLS.UT.SSLContext;

interface

uses
  System.Classes, System.SysUtils, DUnitX.TestFramework, DUnitX.Types,
  DUnitX.InternalDataProvider, DUnitX.TestDataProvider,
  IdGlobal, IdCTypes,
  TaurusTLS.UT.TestClasses, TaurusTLSHeaders_types;

type
  [TestFixture]
  [Category('YourCategory')]
  TYourFixture = class(TOsslBaseFixture)

  end;

implementation


initialization
  TDUnitX.RegisterTestFixture(TYourFixture);

end.
