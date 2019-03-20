import React from "react";

export default class ContentEditable extends React.Component {
  constructor(props) {
    super(props);
    this.handleRef = this.handleRef.bind(this);
  }

  maybeUpdateText() {
    if (!this.el) return;

    if (this.props.text !== this.el.innerText) {
      this.el.innerText = this.props.text;
    }
  }

  componentDidUpdate() {
    this.maybeUpdateText();
  }

  // shouldComponentUpdate(nextProps) {
  //   return Object.keys(nextProps).reduce((a, key) => {
  //     return key !== "text" ? a || this.props[key] !== nextProps[key] : a;
  //   }, false)
  // }

  handlePaste(onPaste) {
    return e => {
      e.preventDefault();
      const text = e.clipboardData.getData("text");
      document.execCommand("insertText", false, text);
      onPaste && onPaste(e);
    };
  }

  handleRef(r) {
    this.el = r;
    this.maybeUpdateText();
    this.props.innerRef && this.props.innerRef(r);
  }

  render() {
    let { text, onChange, onPaste, innerRef, ...props } = this.props;
    return React.createElement("div", {
      ...props,
      contentEditable: "true",
      onInput: e => onChange && onChange(e, e.target.innerText),
      ref: this.handleRef,
      onPaste: this.handlePaste(onPaste)
    });
  }
}
