import React from "react";

export default class ContentEditable extends React.Component {
  constructor(props) {
    super(props);
    this.handleRef = this.handleRef.bind(this);
  }

  maybeNotifySizeChange() {
    if (!this.el || !this.props.onSizeChange) return;

    const rect = this.el.getBoundingClientRect();
    this.props.onSizeChange([rect.width, rect.height]);
  }

  maybeUpdateText() {
    if (!this.el) return;

    if (this.props.text !== this.el.innerText) {
      this.el.innerText = this.props.text;
      this.maybeNotifySizeChange();
    }
  }

  componentDidMount() {
    this.maybeNotifySizeChange();
  }

  componentDidUpdate() {
    this.maybeUpdateText();
  }

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
    this.props.innerRef && this.props.innerRef(r);
    this.maybeUpdateText();
  }

  render() {
    let {
      text,
      onChange,
      onPaste,
      onSizeChange,
      innerRef,
      ...props
    } = this.props;
    return React.createElement("div", {
      ...props,
      contentEditable: "true",
      onInput: e => {
        onChange && onChange(e, e.target.innerText);
        this.maybeNotifySizeChange();
      },
      ref: this.handleRef,
      onPaste: this.handlePaste(onPaste)
    });
  }
}
