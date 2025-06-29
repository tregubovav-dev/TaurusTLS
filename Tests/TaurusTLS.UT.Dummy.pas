(*
  This init contains the single DUnitX test which does nothing.
  This uinit is included to TaursTLS.UT project temporary
  and can be removed at any time.
*)
unit TaurusTLS.UT.Dummy;

interface

uses
  DUnitX.TestFramework, TaurusTLS.UT.TestClasses;

type
  [TestFixture]
  TFixtureDummy = class(TOsslBaseFixture)
  public
    [Test]
    procedure Dummy;
  end;

implementation

{ TFixtureDummy }

procedure TFixtureDummy.Dummy;
begin
  Assert.Pass('This test do nothing.')
end;

initialization
  TDUnitX.RegisterTestFixture(TFixtureDummy);

end.
