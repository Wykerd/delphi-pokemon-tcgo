unit UIButton;

interface

uses
  ExtCtrls, Classes;

type
  TUIButton = class (TImage)
    constructor Create (AOwner : TComponent); override;
  end;

implementation

{$R .\resources\resources.res}

{ TUIButton }

constructor TUIButton.Create(AOwner: TComponent);
begin
  inherited;

end;

end.