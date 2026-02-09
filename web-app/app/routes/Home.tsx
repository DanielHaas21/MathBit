import getApiConfig from '@/apiConfig';
import { Header } from '@/libs/ui/components/Header';
import { BaseLayout } from '@/libs/ui/layouts';
import { Button } from '@headlessui/react';
import { useNavigate } from 'react-router-dom';
import { MathEngineSolveResponse, refresh } from 'web-api-client';

export default function Home() {
  const navigate = useNavigate();
  return (
    <BaseLayout>
      <BaseLayout.Menu>
        <Header route={[]} />
      </BaseLayout.Menu>
      <BaseLayout.Content>
        <Button
          onClick={async () => {
            navigate('/browser/editor');
          }}
        >
          Begin
        </Button>
      </BaseLayout.Content>
    </BaseLayout>
  );
}
