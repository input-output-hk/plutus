"use strict";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");
const path = require("path");

const isDevelopment = process.env.NODE_ENV === "development";

const devtool = isDevelopment ? "eval-source-map" : false;

module.exports = {
    devtool,
    devServer: {
        contentBase: path.join(__dirname, "dist"),
        compress: true,
        port: 8009,
        https: true,
        stats: "errors-warnings",
        proxy: {
            "/api": {
                target: "http://localhost:8080",
            },
        },
    },
    entry: "./entry.js",
    output: {
        path: path.join(__dirname, "dist"),
        pathinfo: true,
        filename: "app.[hash].js",
    },
    module: {
        rules: [
            {
                test: /\.purs$/,
                use: [
                    {
                        loader: "purs-loader",
                        options: {
                            src: [
                                "src/**/*.purs",
                                "generated/**/*.purs",
                                ".spago/*/*/src/**/*.purs",
                                "web-common-plutus/**/*.purs",
                                "web-common-playground/**/*.purs",
                                "web-common/**/*.purs",
                            ],
                            psc: "psa",
                            spago: true,
                            bundle: !isDevelopment,
                            watch: isDevelopment,
                        },
                    },
                ],
            }, {
                test: /\.tsx?$/,
                loader: "ts-loader",
            },
            {
                test: /\.css$/,
                use: [MiniCssExtractPlugin.loader, "css-loader"],
            },
            {
                test: /\.scss$/,
                use: [MiniCssExtractPlugin.loader, "css-loader", "sass-loader"],
            },
            {
                test: /\.(png|svg|jpg|jpeg|gif)$/i,
                type: "asset/resource",
            },
            {
                test: /\.(woff|woff2|eot|ttf|otf)$/i,
                type: "asset/resource",
            },
        ],
    },
    resolve: {
        modules: [
            // We need the second entry for node to be able to
            // locate `node_modules` from client directory when 
            // modules are referenced from inside `web-common`.
            "node_modules", path.resolve(__dirname, "./node_modules")
        ],
        alias: {
            static: path.resolve(__dirname, "./static"),
            src: path.resolve(__dirname, "./src"),
        },
        extensions: [".purs", ".js", ".ts", ".tsx"],
    },
    resolveLoader: {
        modules: [
            "node_modules",
            path.resolve(__dirname, "."),
        ],
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: "web-common/static/index.html",
            favicon: "static/favicon.ico",
            title: "Plutus Playground",
            productName: "plutus-playground",
            googleAnalyticsId: isDevelopment ? "UA-XXXXXXXXX-X" : "UA-119953429-7",
            segmentAnalyticsId: isDevelopment ? "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" : "0CEePM8LJUSpPoo2QGrXHDw4GKg4JFBo",
        }),
        new MiniCssExtractPlugin({
            filename: "[name].[hash].css",
        }),
        new MonacoWebpackPlugin({
            languages: ["haskell"],
        }),
    ],
};
