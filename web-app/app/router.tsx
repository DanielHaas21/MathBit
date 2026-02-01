import { createBrowserRouter } from 'react-router-dom';
import Layout from './Layout';
import Editor from './routes/Editor';
import Browser from './routes/Browser';
import Home from './routes/Home';
import { NotFound } from './routes/NotFound';
import Login from './routes/Login';
import Signup from './routes/Signup';
import Settings from './routes/Settings';

export const router = createBrowserRouter([
  {
    path: '/',
    element: <Layout />,
    children: [
      { path: '/browser/editor', element: <Editor /> },
      { path: '/browser/editor/:id', element: <Editor /> },
      { path: '/browser', element: <Browser /> },
      { path: '/login', element: <Login /> },
      { path: '/signup', element: <Signup /> },
      { path: '/settings', element: <Settings /> },
      { path: '*', element: <NotFound /> },
      { index: true, element: <Home /> },
    ],
  },
]);
