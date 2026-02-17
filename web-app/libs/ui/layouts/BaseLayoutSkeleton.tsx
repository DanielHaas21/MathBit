import { Divider } from '../components';
import { BaseLayout } from './BaseLayout';

export function BaseLayoutSkeleton() {
  return (
    <BaseLayout>
      <BaseLayout.Menu className="gap-0 grid-rows-[auto_1fr_auto] [grid-template-areas:'logo'_'items'_'profile']">
        <div className={'[grid-area:logo'}>
          <div className="min-w-[100px] min-h-[60px] bg-white-500 rounded-lg animate-pulse m-4" />
          <Divider variant="light" thickness="sm"></Divider>
        </div>

        <div className={'[grid-area:items'}>
          <div className="min-w-[100px] min-h-[70px] bg-white-500 rounded-lg animate-pulse m-2" />
          <div className="min-w-[100px] min-h-[70px] bg-white-500 rounded-lg animate-pulse m-2" />
          <div className="min-w-[100px] min-h-[70px] bg-white-500 rounded-lg animate-pulse m-2" />
        </div>

        <div className={'[grid-area:profile'}>
          <Divider variant="light" thickness="sm"></Divider>
          <div className="min-w-[100px] min-h-[70px] bg-white-500 rounded-lg animate-pulse m-4" />
        </div>
      </BaseLayout.Menu>
      <BaseLayout.Content className="p-6 flex flex-col gap-6 ">
        <div className="h-12 bg-white-500 rounded animate-pulse" />
        <div className="h-96 bg-white-500 rounded animate-pulse" />
      </BaseLayout.Content>
    </BaseLayout>
  );
}
