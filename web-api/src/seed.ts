import { initDb, UserRepository } from 'db';
import type { User } from 'db';
import { getAppConfig } from './appConfig';

/**
 * Seed test data for tables
 */
async function seed() {
  const config = getAppConfig();
  const dbConfig = config.db;

  initDb(dbConfig);

  const repo = new UserRepository();

  const peopleNames = [
    'Petr Novák',
    'Jana Svobodová',
    'Martin Dvořák',
    'Lucie Procházková',
    'Tomáš Novotný',
  ];

  const peopleUserNames = ['userN1', 'userN2', 'userN3', 'userN4', 'userN5'];

  const users: User[] = [];
  for (let i = 0; i < 4; i++) {
    const nameParts = peopleNames[i % peopleNames.length]!.split(' ');
    const firstName: string = nameParts[0] || `Test${i + 1}`;
    const lastName: string = nameParts[1] || `User${i + 1}`;
    users.push({
      id: i + 1,
      username: peopleUserNames[i] as string,
      email: `${firstName.toLowerCase()}.${lastName.toLowerCase()}${i + 1}@example.com`,
      password: 'test',
      created: new Date(),
    });
  }

  // for of for async functions
  for (const user of users) {
    await repo.createUser(user);
  }

  console.log('Seeded all test users');
}

seed().catch((err) => {
  console.error(err);
  process.exit(1);
});
