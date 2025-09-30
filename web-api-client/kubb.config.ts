import { defineConfig } from '@kubb/core';
import { pluginOas } from '@kubb/plugin-oas';
import { pluginTs } from '@kubb/plugin-ts';
import { pluginClient } from '@kubb/plugin-client';

import type { Config } from '@kubb/core';

const config: Config = defineConfig({
  root: '.',
  input: {
    path: './schema/schema.json'
  },
  output: {
    path: './generated/',
    clean: true,
    extension: {
      '.ts': ''
    }
  },
  plugins: [
    pluginOas({
      validate: true
    }),
    pluginTs({
      enumType: 'literal',
      dateType: 'date'
    }),
    pluginClient({
      output: {
        path: './api'
      }
    })
  ]
}) as Config;

export default config;
