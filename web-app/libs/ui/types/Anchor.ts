//Dropdown align types, forked from the headless dropdown component since it isnt exported
type Align = 'start' | 'end';
type Placement = 'top' | 'right' | 'bottom' | 'left';
export type AnchorTo = `${Placement}` | `${Placement} ${Align}`;
