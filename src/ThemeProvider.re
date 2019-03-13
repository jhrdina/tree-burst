let theme =
  MaterialUi_Theme.create(
    MaterialUi.ThemeOptions.(
      make(
        ~typography=Typography.make(~useNextVariants=true, ()),
        ~palette=
          PaletteOptions.make(
            ~primary=
              Primary.make(
                ~main="#039be5",
                ~light="#63ccff",
                ~dark="#006db3",
                ~contrastText="#ffffff",
                (),
              ),
            ~secondary=
              Secondary.make(
                ~main="#039be5",
                ~light="#63ccff",
                ~dark="#006db3",
                ~contrastText="#ffffff",
                (),
              ),
            (),
          ),
        (),
      )
    ),
  );

let component = ReasonReact.statelessComponent("ThemeProvider");
let make = children => {
  ...component,
  render: _self =>
    <MaterialUi.ThemeProvider theme> ...children </MaterialUi.ThemeProvider>,
};