// tailwind.config.js
const defaultTheme = require('tailwindcss/defaultTheme')

module.exports = {
  theme: {
    inset: {
      '0': 0,
      auto: 'auto',
      'full': '100%',
    },
    maxHeight: {
      ...defaultTheme.maxHeight,
      'half-screen': '50vh',
      'none': 'none',
    },
    extend: {
      fontFamily: {
        sans: ['Rubik', ...defaultTheme.fontFamily.sans],
      },
      gridTemplateRows: {
        ...defaultTheme.gridTemplateRows,
        gamecolumn: 'auto auto',
      },
      gridTemplateRows: {
        ...defaultTheme.gridTemplateRows,
        gamecolumn: 'auto auto',
      },
    },
  }
}
