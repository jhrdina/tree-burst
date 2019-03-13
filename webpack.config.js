const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const outputDir = path.join(__dirname, "build/");

const isProd = process.env.NODE_ENV === "production";

module.exports = {
  entry: "./src/TreeBurst.js",
  mode: isProd ? "production" : "development",
  output: {
    path: outputDir,
    filename: "Index.js"
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.html",
      inject: false
    })
  ],
  devServer: {
    compress: true,
    contentBase: path.join(__dirname, "src"),
    port: process.env.PORT || 8000,
    historyApiFallback: true
  }
};
