import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

import webpack from 'webpack';

// function getWebServerUrl() {
//   if(!process.env.MARLOWE_WEB_SERVER_URL) {
//     console.log("MARLOWE_WEB_SERVER_URL is missing so the bundle will be purely config.json dependent.")
//     return null;
//   } else {
//     console.log("Checking MARLOWE_WEB_SERVER_URL: " + process.env.MARLOWE_WEB_SERVER_URL);
//     fetch(process.env.MARLOWE_WEB_SERVER_URL + "/healthcheck").catch(function (_err) {
//       console.log("WARNING! The Marlowe Runtime is not responsive!!!");
//     });
//     return process.env.MARLOWE_WEB_SERVER_URL;
//   }
// };


export default function(_env, argv) {
  const develMode = argv.mode == "development";
  // const webServerUrl = getWebServerUrl();

  return {
    experiments: {
      asyncWebAssembly: true
    },
    entry: {
       app: './Main',
    },
    devServer: {
      static: './',
      hot: true,
      port: 8080
    },
    plugins: [
      new webpack.NormalModuleReplacementPlugin(
        /@dcspark\/cardano-multiplatform-lib-nodejs/,
        '@dcspark/cardano-multiplatform-lib-browser'
      ),
      new webpack.EnvironmentPlugin({
        DEVEL_MODE: develMode,
      }),
    ],
    output: {
      filename: 'bundle.js',
      path: path.resolve(__dirname), // , 'public'),
    },
    resolve: {
      modules: ['node_modules'],
      extensions: ['.tsx', '.ts', '.js', '.json']
    },
    module: {
      rules: [
        {
            test: /\.tsx?$/,
            use: 'ts-loader',
            exclude: /node_modules/,
        },
        {
          test: /\.(scss|css)$/,
          sideEffects: true,
          use: [
          {
            // inject CSS to page
            loader: 'style-loader',
          }, {
            // translates CSS into CommonJS modules
            loader: 'css-loader'
          }, {
            // Run postcss actions
            loader: 'postcss-loader',
            options: {
              // `postcssOptions` is needed for postcss 8.x;
              // if you use postcss 7.x skip the key
              postcssOptions: {
                // postcss plugins, can be exported to postcss.config.js
                plugins: function () {
                  return [
                    require('autoprefixer')
                  ];
                }
              }
            }
          }, {
            // compiles Sass to CSS
            loader: 'sass-loader'
          }]
        }
      ]
    }
  };
};

