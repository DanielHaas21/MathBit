import { createBrowserRouter } from 'react-router-dom';
import Layout from './Layout';
import Editor from './routes/Editor';
import Browser from './routes/Browser';
import Home from './routes/Home';

export const router = createBrowserRouter([
  {
    path: '/',
    element: <Layout />,
    children: [
      { path: '/browser/editor', element: <Editor /> },
      { path: '/browser', element: <Browser /> },
      { index: true, element: <Home  /> },
    ],
  },
]);
