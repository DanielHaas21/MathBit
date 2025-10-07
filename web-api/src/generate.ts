import { exec } from 'node:child_process';
import { promises as fs } from 'node:fs';
import path from 'node:path';

/*

This script executes tsoa route generation; after that it checks if a schema.json file exists on the api client 
If it does the script ends, if not it reads ./generated/swagger/swagger.json and writes that as a new schema.json 
This schema is needed fro generating the api client routes

*/

const command = 'tsoa spec && tsoa routes';
const SwaggerPath = path.join(__dirname, '/generated/swagger/swagger.json');
const APIclientPath = path.join(__dirname, '/../../web-api-client/schema/schema.json');

const fileExists = async (filePath: string) => {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
};

const processTSOA = exec(command, (error, stdout, stderr) => {
  if (error) {
    console.error('TSOA generator failed:', error);
    return;
  }
  if (stderr) console.warn('TSOA warnings:', stderr);
});

processTSOA.on('exit', async (code) => {
  if (code !== 0) {
    console.error(`TSOA process exited with code ${code}`);
    process.exit(code ?? 1);
  }

  console.log('TSOA generator finished successfully');
  console.log('Checking for API client schema.json file...');

  const schemaExists = await fileExists(APIclientPath);

  if (schemaExists) {
    console.log('schema.json already exists. Skipping copy.');
  } else {
    console.log('Creating new schema.json file...');
    const swaggerContent = await fs.readFile(SwaggerPath, 'utf-8');

    await fs.mkdir(path.dirname(APIclientPath), { recursive: true });
    await fs.writeFile(APIclientPath, swaggerContent, 'utf-8');

    console.log('schema.json file successfully created!');
  }

  process.exit(0);
});
