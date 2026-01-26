import { createBrowserRouter } from 'react-router-dom';
import Layout from './Layout';
import Editor from './routes/Editor';
import Browser from './routes/Browser';
import Home from './routes/Home';
import { NotFound } from './routes/NotFound';

export const router = createBrowserRouter([
  {
    path: '/',
    element: <Layout />,
    children: [
      { path: '/browser/editor', element: <Editor /> },
      { path: '/browser/editor/:id', element: <Editor /> },
      { path: '/browser', element: <Browser /> },
      { path: '*', element: <NotFound /> },
      { index: true, element: <Home /> },
    ],
  },
]);
