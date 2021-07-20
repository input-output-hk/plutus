"use strict";

module.exports = {
  purge: ["./src/**/*.njk", "./src/**/*.js"],
  darkMode: false, // or 'media' or 'class'
  theme: {
    screens: {
      sm: "640px",
      md: "768px",
      lg: "1024px",
      xl: "1280px",
      widest: "1440px",
    },
    colors: {
      transparent: "transparent",
      current: "currentColor",
      black: "#283346",
      lightgray: "#eeeeee",
      gray: "#dfdfdf",
      green: "#00e39c",
      lightgreen: "#00e872",
      darkgray: "#b7b7b7",
      overlay: "rgba(10,10,10,0.4)",
      white: "#ffffff",
      purple: "#4700c3",
      lightpurple: "#8701fc",
      grayblue: "#f5f9fc",
      darkblue: "#161F2F",
      red: "#e04b4c",
      // NOTE: These colors correspond to the mac button colors
      "mac-red": "#ec6a5e",
      "mac-yellow": "#f3be50",
      "mac-green": "#61c654",
    },
    fontFamily: {
      barlowe: ["barlowe", "sans-serif"],
      comfortaa: ["comfortaa", "sans-serif"],
    },
    // FIXME: we should unify the font sizes
    fontSize: {
      xs: "12px",
      sm: "14px",
      base: "16px",
      lg: "18px",
      xl: "22px",
      "27px": "27px",
      "2xl": "24px",
      "3xl": "36px",
      "4xl": "48px",
      "5xl": "68px",
    },
    borderRadius: {
      sm: "5px",
      DEFAULT: "10px",
      lg: "25px",
      full: "9999px",
    },

    boxShadow: {
      none: "none",
      sm: "0 4px 6px -1px rgba(0,0,0,0.1), 0 2px 4px -1px rgba(0,0,0,0.06)",
      DEFAULT: "0 10px 15px -3px rgba(0,0,0,0.1), 0 4px 6px -2px rgba(0,0,0,0.05)",
      lg: "0 20px 25px -5px rgba(0,0,0,0.2), 0 10px 10px -5px rgba(0,0,0,0.04)",
      xl: "0 25px 50px -12px rgba(0,0,0,0.25)",
      deep: "0 2.5px 5px 0 rgba(0, 0, 0, 0.22)",
    },
    extend: {
      spacing: {
        "5pc": "5%",
        "5vw": "5vw",
        "10vw": "10vw",
      },
      height: {
        logo: "100px",
        "logo-wider": "85px",
      },

      borderWidth: {
        3: "3px",
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
  corePlugins: {
    container: false,
    space: true,
    divideWidth: false,
    divideColor: false,
    divideStyle: false,
    divideOpacity: false,
    accessibility: false,
    appearance: false,
    backgroundAttachment: false,
    backgroundClip: false,
    backgroundColor: true,
    backgroundImage: true,
    gradientColorStops: true,
    backgroundOpacity: false,
    backgroundPosition: true,
    backgroundRepeat: true,
    backgroundSize: true,
    borderCollapse: false,
    borderColor: true,
    borderOpacity: false,
    borderRadius: true,
    borderStyle: false,
    borderWidth: true,
    boxSizing: false,
    cursor: true,
    display: true,
    flexDirection: true,
    flexWrap: false,
    placeItems: false,
    placeContent: false,
    placeSelf: false,
    alignItems: true,
    alignContent: true,
    alignSelf: true,
    justifyItems: false,
    justifyContent: true,
    justifySelf: true,
    flex: true,
    flexGrow: true,
    flexShrink: true,
    order: false,
    float: true,
    clear: false,
    fontFamily: true,
    fontWeight: true,
    height: true,
    lineHeight: true,
    listStylePosition: false,
    listStyleType: false,
    maxHeight: true,
    maxWidth: true,
    minHeight: true,
    minWidth: true,
    objectFit: false,
    objectPosition: false,
    opacity: true,
    outline: true,
    overflow: true,
    overscrollBehavior: false,
    placeholderColor: false,
    placeholderOpacity: false,
    pointerEvents: true,
    position: true,
    inset: true,
    resize: false,
    boxShadow: true,
    ringWidth: true,
    ringOffsetColor: false,
    ringOffsetWidth: false,
    ringColor: true,
    ringOpacity: false,
    fill: false,
    stroke: false,
    strokeWidth: false,
    tableLayout: false,
    textAlign: true,
    textOpacity: false,
    textOverflow: true,
    fontStyle: false,
    textTransform: true,
    textDecoration: true,
    fontSmoothing: false,
    fontVariantNumeric: false,
    letterSpacing: false,
    userSelect: true,
    verticalAlign: false,
    visibility: true,
    whitespace: true,
    wordBreak: false,
    width: true,
    zIndex: true,
    gap: true,
    gridAutoFlow: false,
    gridTemplateColumns: true,
    gridAutoColumns: false,
    gridColumn: true,
    gridColumnStart: true,
    gridColumnEnd: true,
    gridTemplateRows: true,
    gridAutoRows: true,
    gridRow: true,
    gridRowStart: true,
    gridRowEnd: true,
    transform: true,
    transformOrigin: true,
    scale: true,
    rotate: false,
    translate: true,
    skew: false,
    transitionProperty: true,
    transitionTimingFunction: true,
    transitionDuration: true,
    transitionDelay: false,
    animation: true,
  },
};
