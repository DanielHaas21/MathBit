const colors = {
  white: {
    50: '#FFFFFF',
    100: '#FBFCFC',
    200: '#F8F9FA',
    300: '#F4F5F7',
    400: '#F1F2F5',
    500: '#EDEFF2',
    600: '#EAECF0',
    700: '#E6E8ED',
    800: '#E3E5EA',
    900: '#DFE2E8',
    950: '#DCDFE5'
  },
  black: {
    50: '#F4F5F5',
    100: '#DDDFE1',
    200: '#C7C9CD',
    300: '#B0B3B9',
    400: '#999DA5',
    500: '#828791',
    600: '#717680',
    700: '#595D65',
    800: '#45484E',
    900: '#313337',
    950: '#1D1E21'
  },
  brandBlue: {
    50: '#E8F3FC',
    100: '#C2DFF7',
    200: '#9BCBF2',
    300: '#74B7ED',
    400: '#4DA3E8',
    500: '#268FE3',
    600: '#1978C4',
    700: '#1A5787',
    800: '#0D3E65',
    900: '#0A3150',
    950: '#051929'
  },
  brandOrange: {
    50: '#FFF3EB',
    100: '#FFE5D3',
    200: '#FFD7BC',
    300: '#FFC9A4',
    400: '#FFBC8D',
    500: '#FFAE75',
    600: '#FFA05E',
    700: '#FF9246',
    800: '#FF842F',
    900: '#FF7617',
    950: '#FF6901'
  },
  primary: {
    base: '#0D3E65',
    stroke: '#0A3150',
    hover: '#14619D',
    disabled: '#CFD8E0'
  },
  secondary: {
    base: '#FF6901'
  },
  text: {
    black: '#1D1E21',
    grey: '#717680',
    white: '#FFFFFF'
  },
  background: {
    text: '#FFFFFF',
    page: '#F4F5F7',
    inputs: '#F4F5F5'
  },
  stroke: {
    base: '#DDDFE1'
  },
  success: {
    bg: '#ECFDF5',
    icon: '#12B76A',
    text: '#027A48'
  },
  error: {
    bg: '#FEEDEC',
    icon: '#F04438',
    text: '#B42318'
  },
  warn: {
    bg: '#FEF6EB',
    icon: '#F79009',
    text: '#B54708'
  },
  link: {
    base: '#3B82F6',
    hover: '#1D4ED8'
  },
  overlays: {
    // used for backdrops in modals etc.
    25: 'rgba(29,30,33,0.25)', // matches black[950] with 25% opacity
    50: 'rgba(29,30,33,0.5)', // matches black[950] with 50% opacity
    75: 'rgba(29,30,33,0.75)' // matches black[950] with 75% opacity
  }
};

colors.primary.base = colors.brandBlue[800];
colors.primary.stroke = colors.brandBlue[900];
colors.primary.hover = colors.brandBlue[700];
colors.secondary.base = colors.brandOrange[950];

colors.text.black = colors.black[950];
colors.text.grey = colors.black[600];
colors.text.white = colors.white[50];

colors.background.text = colors.white[50];
colors.background.page = colors.white[300];
colors.background.inputs = colors.black[50];

colors.stroke.base = colors.black[100];

export default colors;
