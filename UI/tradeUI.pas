unit tradeUI;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, DBXJSON,
  Controls, ExtCtrls, UIContainer;

type
  TTradeUI = class (TUIContainer)
    constructor Create (AOwner: TComponent); override;
    property
  end;

implementation

{ TTradeUI }

constructor TTradeUI.Create(AOwner: TComponent);
begin
  inherited;

end;

end.