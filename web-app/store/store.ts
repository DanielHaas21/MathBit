import { configureStore } from '@reduxjs/toolkit';
import UserSlice from './slices/UserState';

export const store = configureStore({
  reducer: {
    User: UserSlice,
  },
});

// config types
export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;
