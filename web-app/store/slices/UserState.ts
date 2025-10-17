import { createSlice, PayloadAction } from '@reduxjs/toolkit';
// Same as the one in web-api
export interface UserProfile {
  username?: string | null | undefined;
  email?: string | null | undefined;
  id?: number | null;
  firstName?: string | null | undefined;
  lastName?: string | null | undefined;
}

export interface UserState {
  accessToken: string | null;
  user: UserProfile | null;
}

const initialState: UserState = {
  user: {
    id: null,
    firstName: undefined,
    lastName: undefined,
    email: undefined,
    username: undefined,
  },
  accessToken: null,
};

const UserSlice = createSlice({
  name: 'User',
  initialState,
  reducers: {
    login: (state, action: PayloadAction<UserState>) => {
      state.user = action.payload.user;
      state.accessToken = action.payload.accessToken;
    },
    logout: (state) => {
      state.user = null;
      state.accessToken = null;
    },
  },
});

export const { login, logout } = UserSlice.actions;

export default UserSlice.reducer;
