import { Header } from '@/libs/ui/components/Header';
import { BaseLayout } from '@/libs/ui/layouts';

export default function Browser() {
  return (
    <BaseLayout>
      <BaseLayout.Menu>
        <Header route={[{ pageTitle: 'Browser', pageRoute: '/browser' }]} />
      </BaseLayout.Menu>
      <BaseLayout.Content>s</BaseLayout.Content>
    </BaseLayout>
  );
}
