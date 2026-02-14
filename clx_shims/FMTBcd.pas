unit FMTBcd;
{$mode delphi}
interface

type
  TBcd = record
    Precision: Byte;
    SignSpecialPlaces: Byte;
    Fraction: array[0..31] of Byte;
  end;

function BcdToDouble(const Bcd: TBcd): Double;
function BcdToStr(const Bcd: TBcd): string;

implementation

function BcdToDouble(const Bcd: TBcd): Double;
begin
  Result := 0; // Stub
end;

function BcdToStr(const Bcd: TBcd): string;
begin
  Result := '0'; // Stub
end;

end.
